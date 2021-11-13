package typing;

import reporting.Severity;
import reporting.Diagnostic;
import parsing.Parser;
import lexing.Lexer;
import parsing.ast.Program;
import text.SourceFile;
import typing.Traits;
import Util.detuple2;

@:build(util.Auto.build())
class File implements ITypeLookup implements IErrors {
	final errors: Array<Diagnostic>;
	final dir: Dir;
	final path: String;
	var unit: Option<Unit>;
	var source: SourceFile;
	var program: Option<Program>;
	var status: Bool;
	final imports: Array<Import>;
	final imported: Array<{from: ITypeLookup, types: Array<Type>}>;
	final decls: MultiMap<String, TypeDecl>;
	final categories: Array<Category>;

	function new(dir, path, ?unit) {
		errors = [];
		this.dir = dir;
		this.path = path;
		this.unit = Option.fromNull(unit);
		program = None;
		status = false;
		imports = [];
		imported = [];
		decls = new MultiMap();
		categories = [];
	}

	function initSource() {
		source = new SourceFile(path, sys.io.File.getContent(path));
	}

	function parse() {
		detuple2(@var diags, @var tokens, new Lexer(source).tokenize());
		diags.forEach(diag -> errors.push(diag));
		final result = Parser.parse(tokens);
		
		switch result {
			case Modular([], _) | Script([], _): status = (diags == Nil);
			case Modular(errors, _) | Script(errors, _): errors._for(i => error, {
				this.errors.push(error);
				
				if(i == 25) {
					this.errors.push(new Diagnostic({
						severity: Severity.ERROR,
						message: "Too many errors!",
						info: []
					}));
					break;
				}
			});
		}

		program = Some(result);
	}

	function buildImports() {
		program.forEach(prog -> {
			final decls = switch prog {
				case Modular(_, decls2): decls2;
				case Script(_, decls2): decls2.filterMap(decl -> switch decl {
					case SDecl(decl2): decl2;
					default: null;
				});
			};
			var lastWasUse = true;

			for(decl in decls) switch decl {
				case DUse({span: span, kind: kind, generics: typevars}):
					if(!lastWasUse) {
						lastWasUse = true;
						errors.push(new Diagnostic({
							severity: Severity.WARNING,
							message: "Unorganized code",
							info: [
								Spanned({
									span: span,
									message: "All imports should be at the beginning of the file",
									isSecondary: true
								})
							]
						}));
					}

					if(typevars != Nil) {
						throw "NYI!";
					}

					switch kind {
					case Import(spec, from, as):
						imports.push(new Import({
							span: span,
							spec: spec,
							from: Option.fromNull(from),
							as: as._andOr(a => Some(a._2), None)
						}));
					
					case Pragma(span2, pragma):
						status = false;
						errors.push(new Diagnostic({
							severity: Severity.ERROR,
							message: "Unknown pragma",
							info: [
								Spanned({
									span: span2,
									message: 'Unknown pragma `$pragma`',
									isPrimary: true
								})
							]
						}));
						continue;
					}

				default: if(lastWasUse) lastWasUse = false;
			}
		});
	}

	function buildDecls() {
		program.forEach(prog -> {
			final decls = switch prog {
				case Modular(_, decls2): decls2;
				case Script(_, decls2): decls2.filterMap(decl -> switch decl {
					case SDecl(decl2): decl2;
					default: null;
				});
			};

			for(decl in decls) switch decl {
				case DModule(m): this.addTypeDecl(Module.fromAST(this, m));

				case DClass(c): this.addTypeDecl(Class.fromAST(this, c));

				case DProtocol(p): this.addTypeDecl(Protocol.fromAST(this, p));

				case DKind(k): this.addTypeDecl(Kind.fromAST(this, k));

				case DAlias(a): this.addTypeDecl(Alias.fromAST(this, a));

				case DCategory(c): this.categories.push(Category.fromAST(this, c));

				case DUse(_):

				default: errors.push(Errors.unexpectedDecl(this, decl));
			}
		});
	}

	inline function addTypeDecl(decl: TypeDecl) {
		decls.add(decl.name.name, decl);
	}

	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		if(cache.contains(this)) {
			return null;
		} else {
			cache += this;
		}

		return path._match(
			at([[span, name, args], ...rest]) => {//args=args.map(a->a.simplify());
				var finished = true;
				final res: Null<Type> = decls.find(name).map(found -> found.filter(decl ->
					!cache.contains(decl.thisType)
					&& (args.length == 0 || decl.params.length == args.length)
				))._match(
					at(None | Some([])) => {
						if(search == Inside) {
							null;
						} else {
							var res2 = null;
							for(imp in imported) if(!cache.contains(imp)) {
								(if(imp.from == this) {
									this.findType(path, Start, from, 0, List.of(imp));
								} else {
									//trace(imp.from, imp.types);
									imp.from.findType(path, Inside, from, 0, List.of(this, imp));
								})._match(
									at(found!) => {
										if(imp.types.length == 0 || imp.types.contains(found)) {
											//trace(found.fullName(), imp.types.map(t->t.fullName()), span.display());
											res2 = found;
											break;
										}
									},
									_ => {}
								);
							}

							res2._match(
								at(r!) => r,
								_ => if(search == Inside) {
									null;
								} else {
									unit.orElseDo(dir).findType(path, Outside, from, depth, cache);
								}
							);
						}
					},
					at(Some(_), when(depth != 0)) => {
						if(search == Inside) {
							null;
						} else {
							unit.orElseDo(dir).findType(path, Outside, from, depth - 1, cache);
						}
					},
					at(Some([decl])) => switch [args, decl.params] {
						case [[], []]:
							finished = false;
							{t: decl.thisType.t, span: span};
						case [[], _]:
							finished = false;
							{t: decl.thisType.t, span: span}; // should probably curry parametrics but eh
						case [_, []]:
							if(search == Inside) {
								errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
								null;
							} else {
								// error...?
								null;
							}
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								null;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								null;
							} else {
								finished = false;
								{t: TApplied(new Type(decl.thisType.t, span), args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
							}
					},
					at(Some(decls)) => {
						if(args.length == 0) {
							finished = false;
							{t: TMulti(decls.map(t -> new Type(t.thisType.t, span))), span: span};
						} else switch decls.filter(t -> t.params.length == args.length).map(t -> new Type(t.thisType.t, span)) {
							case []:
								//trace(path, span.display());
								errors.push(Errors.invalidTypeApply(span, "No matching candidates were found"));
								null;
							case [type]:
								finished = false;
								{t: TApplied(type, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
							case types:
								finished = false;
								{t: TApplied({t: TMulti(types), span: span}, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
						}
					}
				);

				Util._match([rest, res],
					at([_, null]) => if(search == Inside) null else unit.orElseDo(dir).findType(path, Outside, from, depth, cache),
					at([_, _], when(finished)) => res,
					at([Nil3, _]) => res,
					at([_, type = (_ : Type) => {t: TConcrete(decl)}]) =>
						unit.doOrElse(u => {
							type = {t: TModular(type, u), span: span};
							decl.findType(rest, Inside, from, 0, cache)._or(
								u.findType(rest, Outside, from, 0, cache + decl.thisType)
							);
						}, {
							decl.findType(rest, Inside, from, 0, cache);
						}),
					at([_, type!!]) => {t: TLookup(type, rest, this), span: span}
				);
			},
			_ => throw "bad"+(untyped cache.head():File).path
		);
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function hasErrors() {
		return !status
			|| errors.length != 0
			|| decls.allValues().some(d -> d.hasErrors())
			|| categories.some(c -> c.hasErrors());
	}

	function allErrors() {
		return errors
			.concat(decls.allValues().flatMap(decl -> decl.allErrors()))
			.concat(categories.flatMap(c -> c.allErrors()));
	}


	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		cache += this;
		//trace(cat, this.path);
		for(cat2 in categories) {
			cat2.path=cat2.path.simplify();
			cat2.type=cat2.type.map(t->t.simplify());
		}
		/*if(categories.length!=0) {
			Sys.print("\n");
			trace("=== "+forType.fullName()+" === " + cat.span._and(s=>s.display()));
		}*/
		return cat.t._match(
			at(TModular(t, unit), when(!cache.contains(unit))) => {
				/*function loop(unit_: Unit, cache: List<Unit>) {
					if(cache.contains(unit_)) return [];
					cache += unit_;
					return unit_.files.flatMap(f -> f.categories)
						.concat(unit_.primary.doOrElse(p => p.categories, []))
						.concat(unit_.units.flatMap(u -> loop(u, cache)))
						.concat(unit_._match(
							at(p is Project) => (
								p.files.flatMap(f -> f.categories)
									.concat(p.units.flatMap(u -> loop(u, cache)))
							),
							at(u is Unit) => loop(u, cache),
							_ => throw "bad"
						));
				}*/

				//final r = categories.concat(loop(unit, Nil));
				final files = [];
				unit.gatherFiles(files);
				files.remove(this);
				final r = categories.concat(files.flatMap(f -> f.categories));
				//trace(r.map(c->c.fullName()).join("\n")+"\n");
				r;
			},
			_ => {
				//trace(categories.map(c->c.fullName()));
				categories;
			}
		).filter(c -> {
			//c.path.hasChildType(cat) && c.thisType.simplify().hasChildType(forType)
			if(cat.hasParentType(c.path)) {
				if(forType.hasParentType(c.thisType)) {
					//trace("+", c.fullName());
					true;
				} else {
					//trace("-", forType.hasParentType(c.thisType), c.fullName());
					false;
				}
			} else {
				false;
			}
		})._match(
			at([]) => dir.findCategory(ctx, cat, forType, from, cache),
			at(found) => found
		);
	}
}