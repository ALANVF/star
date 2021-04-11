package compiler;

enum InitList {
	LExprs(e: Array<Expr>);
	LNamed(n: Array<{name: String, expr: Expr}>);
	LNamedInit(b: Array<{name: String, init: InitList}>);
}