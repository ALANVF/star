package typing;

import text.Span;
import parsing.ast.Expr;
import parsing.ast.Ident;
import typing.Traits;

class MultiStaticMethod extends StaticMethod {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var params: Array<{label: Ident, name: Ident, type: Type, value: Option<Expr>}> = [];
	var fuzzyName: String;
	var isUnordered: Bool = false;

	static function fromAST(decl: ITypeDecl, ast: parsing.ast.decls.Method) {
		final method = new MultiStaticMethod({
			decl: decl,
			span: ast.span,
			params: null,    // hack for partial initialization
			fuzzyName: null, // hack for partial initialization
			ret: null,       // hack for partial initialization
			body: ast.body.map(body -> body.stmts())
		});

		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = method.makeTypePath(p.type);
				return switch [p.label, p.name] {
					case [Some(l), Some(n)]: {label: l, name: n, type: type, value: p.value};
					case [Some(l), None]: {label: l, name: l, type: type, value: p.value};
					case [None, Some(n)]: {label: new Ident(n.span, "_"), name: n, type: type, value: p.value};
					case [None, None]:
						final span = {
							final s = p.type.span();
							Span.at(s.start, s.source.toNull());
						};
						final ident = new Ident(span, "_");
						{label: ident, name: ident, type: type, value: p.value};
				}
			});
			default: throw "Error!";
		};

		method.params = params;
		method.fuzzyName = params.map(p -> p.label.name + ":").join(" ");
		method.ret = ast.ret.map(ret -> method.makeTypePath(ret));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(method, a))) {
			method.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsStatic:
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.fuzzyName, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.isMain = true;

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.isGetter = true;

			case IsSetter: method.isSetter = true;

			case IsUnordered: method.isUnordered = true;

			case IsNative(_) if(method.native.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.fuzzyName, "native", span));
			case IsNative(sym): method.native = Some(sym);

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}

	override function findType(path: LookupPath, absolute = true, cache: List<{}> = Nil): Option<Type> {
		return path._match(
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return decl.findType(path, true, cache);
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
			_ => if(absolute) decl.findType(path, true, cache) else None
		);
	}

	function methodName() {
		return fuzzyName.replaceAll(" ", "");
	}
}