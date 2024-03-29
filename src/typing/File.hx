package typing;

import reporting.Severity;
import errors.Error;
import parsing.Parser;
import lexing.Lexer;
import parsing.ast.Program;
import text.SourceFile;
import typing.Traits;
import Util.detuple;

@:publicFields
class File implements ITypeLookup implements IErrors {
	final errors: Array<Error>;
	final dir: Dir;
	final path: String;
	var unit: Option<Unit>;
	var source: SourceFile;
	var program: Option<Program>;
	var status: Bool;
	final imports: Array<Import>;
	final imported: Array<{from: ITypeLookup, types: Array<Type>}>;
	final decls: MultiMap<String, TypeDecl>;
	final sortedDecls: Array<TypeDecl>;
	final categories: Array<Category>;

	function new(dir: Dir, path: String, ?unit: Unit) {
		errors = [];
		this.dir = dir;
		this.path = path;
		this.unit = Option.fromNull(unit);
		program = None;
		status = false;
		imports = [];
		imported = [];
		decls = new MultiMap();
		sortedDecls = [];
		categories = [];
	}

	function initSource() {
		source = new SourceFile(path, sys.io.File.getContent(path));
	}

	function parse() {
		detuple(@var [diags, tokens] = new Lexer(source).tokenize());
		diags.forEach(diag -> errors.push(diag));
		final result = Parser.parse(tokens);
		
		switch result {
			case Modular([], _) | Script([], _): status = diags.match(Nil);
			case Modular(errors, _) | Script(errors, _): for(i => error in errors) {
				this.errors.push(error);
				
				if(i == 25) {
					this.errors.push(TooManyErrors);
					break;
				}
			}
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
						errors.push(Type_UnorganizedCode(span));
					}

					if(typevars != Nil) {
						throw "NYI!";
					}

					switch kind {
					case Import(spec, from, as):
						imports.push({
							span: span,
							spec: spec,
							from: Option.fromNull(from),
							as: as._andOr(a => Some(a._2), None)
						});
					
					case Pragma(span2, pragma):
						status = false;
						errors.push(Type_UnknownPragma(pragma, span2));
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

				default: errors.push(Type_UnexpectedDeclInFile(this, decl));
			}
		});
	}

	inline function addTypeDecl(decl: TypeDecl) {
		decls.add(decl.name.name, decl);
		sortedDecls.push(decl);
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

							res2 ?? if(search == Inside) {
								null;
							} else {
								unit.orElseDo(dir).findType(path, Outside, from, depth, cache);
							};
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
								errors.push(Type_InvalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
								null;
							} else {
								// error...?
								null;
							}
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Type_InvalidTypeApply(span, "Too many arguments"));
								null;
							} else if(args.length < params.length) {
								errors.push(Type_InvalidTypeApply(span, "Not enough arguments"));
								null;
							} else {
								finished = false;
								{t: TApplied(new Type(decl.thisType.t, span), args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth) ?? {
										errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									},
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
								errors.push(Type_InvalidTypeApply(span, "No matching candidates were found"));
								null;
							case [type]:
								finished = false;
								{t: TApplied(type, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth) ?? {
										errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									},
									_ => arg
								))), span: span};
							case types:
								finished = false;
								{t: TApplied({t: TMulti(types), span: span}, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth) ?? {
										errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
										arg;
									},
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
							decl.findType(rest, Inside, from, 0, cache)
							?? u.findType(rest, Outside, from, 0, cache + decl.thisType);
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
			|| sortedDecls.some(d -> d.hasErrors())
			|| categories.some(c -> c.hasErrors());
	}

	function allErrors() {
		return errors
			.concat(sortedDecls.flatMap(decl -> decl.allErrors()))
			.concat(categories.flatMap(c -> c.allErrors()));
	}


	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		if(cache.contains(this)) return [];
		cache += this;
		//if(cat.fullName()=="Star.Core.InPlace"&&forType.fullName()=="Star.Core.Linked.List[T]"&&this.path=="C:/Users/alani/Documents/GitHub/star/stdlib/Star/Core/InPlace/Linked.List[T]+InPlace.star")
		///*if(!this.path.startsWith("C:/Users/alani/Documents/GitHub/star/stdlib"))*/trace(cat.fullName()+" for "+forType.fullName(), this.path);
		for(cat2 in categories) {
			cat2.path = cat2.path.simplify();
			cat2.type = cat2.type?.simplify();
		}
		/*if(categories.length!=0) {
			Sys.print("\n");
			trace("=== "+forType.fullName()+" === " + cat.span?.display());
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
		).unique().filter(c -> {
			/*if(cat.fullName()=="Typer.Type"&&forType.fullName()=="Star.Core.Array.[Typer.Member]")
			//if(!this.path.startsWith("C:/Users/alani/Documents/GitHub/star/stdlib"))
			{
				trace("");
				trace(c.fullName(),c.thisType.fullName(),forType.fullName());
				trace(
					cat.hasParentType(c.path), c.path.hasChildType(cat),
					forType.hasParentType(c.thisType), c.thisType.getMostSpecific().hasChildType(forType)
				);
				trace("");
			}*/
			c.thisType=c.thisType.simplify();
			//c.path.hasChildType(cat) && c.thisType.simplify().hasChildType(forType)
			/*if(cat.hasParentType(c.path)) {
				if(forType.hasParentType(c.thisType)) {
					//trace("+", c.fullName());
					true;
				} else {
					//trace("-", forType.hasParentType(c.thisType), c.fullName());
					false;
				}
			} else {
				false;
			}*/
			cat.hasParentType(c.path) && c.path.hasChildType(cat) &&
				forType.hasParentType(c.thisType) && c.thisType.getMostSpecific().hasChildType(forType);
		})._match(
			at([]) => dir.findCategory(ctx, cat, forType, from, cache),
			at(found) => found
		);
	}
}