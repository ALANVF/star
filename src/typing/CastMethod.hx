package typing;

@:structInit
class CastMethod extends Method {
	var typevars = new MultiMap<String, TypeVar>();
	var type: Type;

	static function fromAST(decl, ast: parsing.ast.decls.Method) {
		final method: CastMethod = {
			decl: decl,
			span: ast.span,
			type: null, // hack for partial initialization
			ret: null,
			body: ast.body?.stmts()
		};

		method.type = switch ast.spec.of {
			case Cast(type): method.makeTypePath(type);
			default: throw "Error!";
		};

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(method, a))) {
			method.typevars.add(typevar.name.name, typevar);
		}

		final typeName = method.type.simpleName();

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Type_InvalidAttribute(method, typeName, "static", span));
			
			case IsHidden(_) if(method.hidden != null): method.errors.push(Type_DuplicateAttribute(method, typeName, "hidden", span));
			case IsHidden(None): method.hidden = None;
			case IsHidden(Some(outsideOf)): method.hidden = Some(decl.makeTypePath(outsideOf));

			case IsMain: method.errors.push(Type_InvalidAttribute(method, typeName, "main", span));

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.errors.push(Type_InvalidAttribute(method, typeName, "getter", span));

			case IsSetter: method.errors.push(Type_InvalidAttribute(method, typeName, "setter", span));

			case IsUnordered: method.errors.push(Type_InvalidAttribute(method, typeName, "unordered", span));

			case IsNative(_) if(method.native != null): method.errors.push(Type_DuplicateAttribute(method, typeName, "native", span));
			case IsNative(sym): method.native = sym;

			case IsInline: method.isInline = true;

			case IsAsm: method.isAsm = true;

			case IsMacro: method.isMacro = true;
		}

		return method;
	}
	

	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		return BaseMethod._findType(this, path, from, depth);
	}

	function methodName() {
		return type.simpleName();
	}

	override function hasErrors() {
		return super.hasErrors()
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(typevars.allValues().flatMap(g -> g.allErrors()));
	}
}