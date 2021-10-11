package typing;

class Deinit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		return new Deinit({
			decl: decl,
			span: ast.span,
			body: ast.body.stmts()
		});
	}

	inline function declName() {
		return "deinitializer";
	}
}