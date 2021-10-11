package typing;

import parsing.ast.Expr;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

class MultiInit extends Init {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var params: Array<{label: Ident, name: Ident, type: Type, ?value: Expr}> = [];
	var fuzzyName: String;
	var isUnordered: Bool = true;

	static function fromAST(decl: ITypeDecl, ast: parsing.ast.decls.Init): MultiInit {
		final init = new MultiInit({
			decl: decl,
			span: ast.span,
			params: null,    // hack for partial initialization
			fuzzyName: null, // hack for partial initialization
			body: ast.body.map(body -> body.stmts())
		});

		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = init.makeTypePath(p.type);
				return switch [p.label, p.name] {
					case [Some(l), Some(n)]: {label: l, name: n, type: type, value: p.value.toNull()};
					case [Some(l), None]: {label: l, name: l, type: type, value: p.value.toNull()};
					case [None, Some(n)]: {label: new Ident(n.span, "_"), name: n, type: type, value: p.value.toNull()};
					case [None, None]:
						final span = {
							final s = p.type.span();
							Span.at(s.start, s.source.toNull());
						};
						final ident = new Ident(span, "_");
						{label: ident, name: ident, type: type, value: p.value.toNull()};
				}
			});
			default: throw "Error!";
		};

		init.params = params;
		init.fuzzyName = params.map(p -> p.label.name + ":").join(" ");


		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(init, a))) {
			init.typevars.add(typevar.name.name, typevar);
		}
		
		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(init.hidden.isSome()): init.errors.push(Errors.duplicateAttribute(init, init.fuzzyName, "hidden", span));
			case IsHidden(None): init.hidden = Some(None);
			case IsHidden(Some(outsideOf)): init.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsNoinherit: init.noInherit = true;

			case IsUnordered: init.isUnordered = true;

			case IsNative(_) if(init.native.isSome()): init.errors.push(Errors.duplicateAttribute(init, init.fuzzyName, "native", span));
			case IsNative(sym): init.native = Some(sym);

			case IsAsm: init.isAsm = true;
			
			case IsMacro: init.isMacro = true;
		}

		return init;
	}


	override function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		return BaseMethod._findType(this, path, depth);
	}

	/*override function findTypeOld(path: LookupPath, absolute = true, cache: List<{}> = Nil): Option<Type> {
		return path._match(
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return decl.findTypeOld(path, true, cache);
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
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
					case [_, None]: res;
				}
			},
			_ => if(absolute) decl.findTypeOld(path, true, cache) else None
		);
	}*/

	function methodName() {
		return fuzzyName.replaceAll(" ", "");
	}
}