package typing;

class CastMethod extends Method {
	@:ignore final generics = new MultiMap<String, Generic>();
	var type: Type;

	static function fromAST(decl, ast: parsing.ast.decls.Method) {
		final method = new CastMethod({
			decl: decl,
			span: ast.span,
			type: switch ast.spec.of {
				case Cast(type): decl.makeTypePath(type);
				default: throw "Error!";
			},
			ret: None,
			body: ast.body.map(body -> body.stmts)
		});

		for(generic in ast.generics.mapArray(Generic.fromAST.bind(decl, _))) {
			method.generics.add(generic.name.name, generic);
		}

		final typeName = method.type.simpleName();

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Errors.invalidAttribute(method, typeName, "static", span));
			
			case IsHidden(_) if(method.hidden.isSome()): method.errors.push(Errors.duplicateAttribute(method, typeName, "hidden", span));
			case IsHidden(None): method.hidden = Some(None);
			case IsHidden(Some(outsideOf)): method.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsMain: method.errors.push(Errors.invalidAttribute(method, typeName, "main", span));

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.errors.push(Errors.invalidAttribute(method, typeName, "getter", span));

			case IsSetter: method.errors.push(Errors.invalidAttribute(method, typeName, "setter", span));

			case IsUnordered: method.errors.push(Errors.invalidAttribute(method, typeName, "unordered", span));

			case IsNative(_) if(method.native.isSome()): method.errors.push(Errors.duplicateAttribute(method, typeName, "native", span));
			case IsNative(sym): method.native = Some(sym);

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}

	override function hasErrors() {
		return super.hasErrors() || generics.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(generics.allValues().flatMap(g -> g.allErrors()));
	}
}