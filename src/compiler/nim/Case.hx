package compiler.nim;

enum Case<T> {
	COf(value: Expr, body: Array<T>);
	COfAny(values: Exprs, body: Array<T>);
	CElif(cond: Expr, body: Array<T>);
}