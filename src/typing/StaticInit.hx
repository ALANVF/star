package typing;

import text.Span;

@:structInit
class StaticInit extends EmptyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.BaseMethod) {
		final declSpan = ast.attrs.exists(IsStatic) ? Span.range(ast.span, ast.attrs.get(IsStatic)) : ast.span;
		
		return ({
			decl: decl,
			span: declSpan,
			body: ast.body.stmts()
		}:StaticInit);
	}

	function declName() {
		return "static initializer";
	}
}