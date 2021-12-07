package typing;

import parsing.ast.Ident;

class SingleMethod extends Method {
	final name: Ident;

	static function fromAST(decl, ast: parsing.ast.decls.Method) {
		final method = new SingleMethod({
			decl: decl,
			span: ast.span,
			name: switch ast.spec.of {
				case Single(name): name;
				default: throw "Error!";
			},
			ret: ast.ret.toNull()._and(ret => decl.makeTypePath(ret)),
			body: ast.body.toNull()._and(body => body.stmts())
		});

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Errors.invalidAttribute(method, method.name.name, "static", span));
			
			case IsHidden(_) if(method.hidden != null): method.errors.push(Errors.duplicateAttribute(method, method.name.name, "hidden", span));
			case IsHidden(None): method.hidden = None;
			case IsHidden(Some(outsideOf)): method.hidden = Some(decl.makeTypePath(outsideOf));

			case IsMain: method.isMain = true;

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.isGetter = true;

			case IsSetter: method.errors.push(Errors.invalidAttribute(method, method.name.name, "setter", span));

			case IsUnordered: method.errors.push(Errors.invalidAttribute(method, method.name.name, "unordered", span));

			case IsNative(_) if(method.native != null): method.errors.push(Errors.duplicateAttribute(method, method.name.name, "native", span));
			case IsNative(sym): method.native = sym;

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}

	function methodName() {
		return name.name;
	}
}