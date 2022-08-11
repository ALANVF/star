package typing;

@:structInit
class Deinit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		return ({
			decl: decl,
			span: ast.span,
			body: ast.body.stmts()
		}:Deinit);
	}

	function declName() {
		return "deinitializer";
	}
}