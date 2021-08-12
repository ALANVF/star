package typing;

import text.Span;
import parsing.ast.Expr;
import parsing.ast.Ident;
import typing.Traits;

class MultiMethod extends Method {
	@:ignore final generics = new MultiMap<String, Generic>();
	final params: Array<{label: Ident, name: Ident, type: Type, value: Option<Expr>}> = [];
	final fuzzyName: String;
	var isUnordered: Bool = false;

	static function fromAST(decl: ITypeDecl, ast: parsing.ast.decls.Method) {
		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = decl.makeTypePath(p.type);
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
		final method = new MultiMethod({
			decl: decl,
			span: ast.span,
			params: params,
			fuzzyName: params.map(p -> p.label + ":").join(" "),
			ret: ast.ret.map(ret -> decl.makeTypePath(ret)),
			body: ast.body.map(body -> body.stmts())
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(decl, _))) {
			method.generics.add(generic.name.name, generic);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Errors.invalidAttribute(method, method.fuzzyName, "static", span));
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.fuzzyName, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.errors.push(Errors.invalidAttribute(method, method.fuzzyName, "main", span));

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
}