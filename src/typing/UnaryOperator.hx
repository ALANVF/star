package typing;

@:structInit
class UnaryOperator extends Operator {
	var op: UnaryOp;

	static function fromAST(decl, op, ast: parsing.ast.decls.Operator) {
		return ({
			decl: decl,
			span: ast.span,
			op: op,
			opSpan: ast.symbolSpan,
			ret: ast.ret._and(ret => decl.makeTypePath(ret)),
			body: ast.body?.stmts()
		}:UnaryOperator);
	}

	function methodName() {
		return opName();
	}

	inline function opName() {
		return op.symbol();
	}
}