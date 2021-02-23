package typing;

import parsing.ast.Expr;
import text.Span;
import parsing.ast.Ident;

class MultiInit extends Init {
	@:ignore final generics = new MultiMap<String, Generic>();
	final params: Array<{label: Ident, name: Ident, type: Type, value: Option<Expr>}> = [];
	final fuzzyName: String;
	var isUnordered: Bool = true;

	static function fromAST(decl: ITypeDecl, ast: parsing.ast.decls.Init) {
		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> {
				final type = decl.makeTypePath(p.type);
				return switch [p.label, p.name] {
					case [Some(l), Some(n)]: {label: l, name: n, type: type, value: p.value};
					case [Some(l), None]: {label: l, name: l, type: type, value: p.value};
					case [None, Some(n)]: {label: new Ident(n.span, "_"), name: n, type: type, value: p.value};
					case [None, None]:
						final span = switch p.type {
							case Nil: throw "Error!";
							case Cons(Named(span2, _, _) | Blank(span2, _), _):
								Span.at(span2.start.advance(-1), span2.source.toNull());
						};
						final ident = new Ident(span, "_");
						{label: ident, name: ident, type: type, value: p.value};
				}
			});
			default: throw "Error!";
		};
		final init = new MultiInit({
			decl: decl,
			span: ast.span,
			params: params,
			fuzzyName: params.map(p -> p.label + ":").join(" "),
			body: ast.body.map(body -> body.stmts)
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(decl, _))) {
			init.generics.add(generic.name.name, generic);
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
		}

		return init;
	}
}