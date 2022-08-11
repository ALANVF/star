package typing;

@:structInit
class DefaultInit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		return ({
			decl: decl,
			span: ast.span,
			body: ast.body.stmts()
		}:DefaultInit);
	}

	function declName() {
		return "default initializer";
	}
}