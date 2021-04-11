package compiler;

enum VarDeclKind {
	VVar(name: String);
	VUnpack(names: Array<String>);
}

enum VarDeclExprKind {
	VAssign(expr: Expr);
	VCall(args: Array<Expr>);
	VInitList(l: InitList);
}

@:build(util.Auto.build())
class VarDecl {
	var attrs: Attrs = [];
	var kind: VarDeclKind;
	var expr: VarDeclExprKind;
}