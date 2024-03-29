package typing;

import text.Span;
import parsing.ast.Expr;
import parsing.ast.Ident;
import typing.Traits;

@:structInit
class MultiMethod extends Method {
	var typevars = new MultiMap<String, TypeVar>();
	var params: MultiParams = [];
	var fuzzyName: String;
	var isUnordered: Bool = false;

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Method) {
		final method: MultiMethod = {
			decl: decl,
			span: ast.span,
			params: null,    // hack for partial initialization
			fuzzyName: null, // hack for partial initialization
			ret: null,       // hack for partial initialization
			body: ast.body?.stmts()
		};

		final params = switch ast.spec.of {
			case Multi(params2): params2.map(p -> MultiParam.fromUntyped(method, p));
			default: throw "Error!";
		};

		method.params = params;
		method.fuzzyName = params.map(p -> p.label.name + ":").join(" ");
		method.ret = ast.ret._and(ret => method.makeTypePath(ret));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(method, a))) {
			method.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsStatic: method.errors.push(Type_InvalidAttribute(method, method.fuzzyName, "static", span));
			
			case IsHidden(_) if(method.hidden != null): method.errors.push(Type_DuplicateAttribute(method, method.fuzzyName, "hidden", span));
			case IsHidden(None): method.hidden = None;
			case IsHidden(Some(outsideOf)): method.hidden = Some(decl.makeTypePath(outsideOf));

			case IsMain: method.errors.push(Type_InvalidAttribute(method, method.fuzzyName, "main", span));

			case IsNoinherit: method.noInherit = true;

			case IsGetter: method.isGetter = true;

			case IsSetter: method.isSetter = true;

			case IsUnordered: method.isUnordered = true;

			case IsNative(_) if(method.native != null): method.errors.push(Type_DuplicateAttribute(method, method.fuzzyName, "native", span));
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
		return fuzzyName.replaceAll(" ", "");
	}
}