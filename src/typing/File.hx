package typing;

import reporting.Severity;
import reporting.Diagnostic;
import parsing.Parser;
import lexing.Lexer;
import parsing.ast.Program;
import text.SourceFile;
import typing.Traits;

@:build(util.Auto.build())
class File implements IErrors {
	final errors: Array<Diagnostic>;
	final dir: Dir;
	final path: String;
	var unit: Option<Unit>;
	var source: SourceFile;
	var program: Option<Program>;
	var status: Bool;
	final imports: Array<Import>;
	final imported: Array<ILookupType>;
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
		try {
			final result = Parser.parse(new Lexer(source).tokenize());
			
			switch result {
				case Modular([], _) | Script([], _): status = true;
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
		} catch(diag: Diagnostic) {
			errors.push(diag);
		}
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

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}

		return path._match(
			/*at([[typeName, _]]) => switch decls.find(typeName) {
				case None: if(absolute) dir.findType(path, true, cache) else None;
				case Some([decl]): Some(decl.thisType);
				case Some(found): Some(new Type(TMulti(found.map(d -> d.thisType))));
			},
			at([[typeName, _], ...rest]) => switch decls.find(typeName) {
				case None: if(absolute) dir.findType(path, true, cache) else None;
				case Some([decl]): decl.findType(rest, false, cache);
				case Some(found): throw "NYI!";
			},*/
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch decls.find(typeName) {
					case None:
						var res2 = null;
						if(absolute) {
							for(imp in imported) imp.findType(path, true, cache.prepend(imp))._match(
								at(Some(found)) => {
									res2 = Some(found);
									break;
								},
								_ => {}
							);
						}

						res2._match(
							at(r!) => r,
							_ => return if(absolute) unit.orElse(dir).findType(path, true, cache) else None
						);
					
					case Some([type]): switch [args, type.params] {
						case [[], _]: Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
							None;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								Some({t: TApplied(type.thisType, args), span: span});
							}
					}
					case Some(found):
						if(args.length == 0) {
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(g -> g.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No matching candidates were found"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [_, None]: if(absolute) unit.orElse(dir).findType(path, true, cache) else None;
					case [Nil3, _]: res;
					case [_, Some(type={t: TConcrete(decl)})]:
						unit.doOrElse(u => {
							type = new Type(TModular(type, u));
							type.findType(path, false, cache);
						}, {
							type.findType(rest, false, cache);
						});
					/*case [_, Some({t: TModular(t, u)})]:
						t.findType(rest, false, cache).orDo(u.findType(rest, false, cache.prepend(t)));*/
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => if(absolute) unit.orElse(dir).findType(path, true, cache) else None
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
}