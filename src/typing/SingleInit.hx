package typing;

import parsing.ast.Ident;

class SingleInit extends Init {
	final name: Ident;

	static function fromAST(decl, ast: parsing.ast.decls.Init) {
		final init = new SingleInit({
			decl: decl,
			span: ast.span,
			name: switch ast.spec.of {
				case Single(name): name;
				default: throw "Error!";
			},
			body: ast.body.map(body -> body.stmts())
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(init.hidden.isSome()): init.errors.push(Errors.duplicateAttribute(init, init.name.name, "hidden", span));
			case IsHidden(None): init.hidden = Some(None);
			case IsHidden(Some(outsideOf)): init.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsNoinherit: init.noInherit = true;

			case IsUnordered: init.errors.push(Errors.invalidAttribute(init, init.name.name, "unordered", span));

			case IsNative(_) if(init.native.isSome()): init.errors.push(Errors.duplicateAttribute(init, init.name.name, "native", span));
			case IsNative(sym): init.native = Some(sym);

			case IsAsm: init.isAsm = true;
			
			case IsMacro: init.isMacro = true;
		}

		return init;
	}
}