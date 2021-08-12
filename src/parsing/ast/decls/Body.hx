package parsing.ast.decls;

import text.Span;

@:using(parsing.ast.decls.Body.BodyTools)
enum Body {
	BBlock(b: Block);
	BArrow(_: Span, s: Stmt);
}

@:publicFields
class BodyTools {
	public static inline function stmts(self: Body) return switch self {
		case BBlock(b): b.stmts;
		case BArrow(_, s): [s];
	}
}