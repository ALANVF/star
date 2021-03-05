package typing;

import parsing.ast.Ident;

class SingleStaticMethod extends StaticMethod {
	final name: Ident;

	static function fromAST(decl, ast: parsing.ast.decls.Method) {
		final method = new SingleStaticMethod({
			decl: decl,
			span: ast.span,
			name: switch ast.spec.of {
				case Single(name): name;
				default: throw "Error!";
			},
			ret: ast.ret.map(ret -> decl.makeTypePath(ret)),
			body: ast.body.map(body -> body.stmts)
		});

		for(attr => span in ast.attrs) switch attr {
			case IsStatic:
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.name.name, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.isMain = true;

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.isGetter = true;

			case IsSetter: method.errors.push(Errors.invalidAttribute(method, method.name.name, "setter", span));

			case IsUnordered: method.errors.push(Errors.invalidAttribute(method, method.name.name, "unordered", span));

			case IsNative(_) if(method.native.isSome()): method.errors.push(Errors.duplicateAttribute(method, method.name.name, "native", span));
			case IsNative(sym): method.native = Some(sym);

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}
}