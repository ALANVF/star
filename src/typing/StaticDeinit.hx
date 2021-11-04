package typing;

import text.Span;

class StaticDeinit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		final declSpan = ast.attrs.exists(IsStatic) ? Span.range(ast.span, ast.attrs.get(IsStatic)) : ast.span;
		
		return new StaticDeinit({
			decl: decl,
			span: declSpan,
			body: ast.body.stmts()
		});
	}

	function declName() {
		return "static deinitializer";
	}
}