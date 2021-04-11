package compiler;

enum InitCtor {
	CCall(args: Array<Expr>);
	CInitList(l: InitList);
}