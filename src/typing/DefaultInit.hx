package typing;

class DefaultInit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		return new DefaultInit({
			decl: decl,
			span: ast.span,
			body: ast.body.stmts
		});
	}
}