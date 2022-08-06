package typing;

import errors.Error;
import parsing.ast.Ident;

class BinaryOperator extends Operator {
	@ignore var typevars = new MultiMap<String, TypeVar>();
	var op: BinaryOp;
	var paramName: Ident;
	var paramType: Type;

	static function fromAST(decl, op, paramName, paramType, ast: parsing.ast.decls.Operator) {
		final oper = new BinaryOperator({
			decl: decl,
			span: ast.span,
			op: op,
			opSpan: ast.symbolSpan,
			paramName: paramName,
			paramType: null, // hack for partial initialization
			ret: null,       // hack for partial initialization
			body: ast.body._and(body => body.stmts())
		});

		oper.paramType = oper.makeTypePath(paramType);
		oper.ret = ast.ret._and(ret => oper.makeTypePath(ret));

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(oper, a))) {
			oper.typevars.add(typevar.name.name, typevar);
		}

		return oper;
	}

	function methodName() {
		return opName();
	}

	override function hasErrors() {
		return super.hasErrors()
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(typevars.allValues().flatMap(g -> g.allErrors()));
	}

	inline function opName() {
		return op.symbol();
	}


	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		return BaseMethod._findType(this, path, from, depth);
	}
}