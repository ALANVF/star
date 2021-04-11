package compiler;

enum Cond {
	CExpr(e: Expr);
	CVarDecl(v: VarDecl);
}

enum Stmt {
	SExpr(expr: Expr);
	SVarDecl(v: VarDecl);
	SBlock(b: Block);
	SNone;
	SLabel(name: String, stmt: Stmt);
	SCase(expr: Expr, stmt: Stmt);
	SDefault(stmt: Stmt);
	SIf(isConstexpr: Bool, init: Option<VarDecl>, cond: Cond, ifTrue: Stmt, ifFalse: Option<Stmt>);
	SSwitch(init: Option<VarDecl>, cond: Cond, stmt: Stmt);
	SWhile(cond: Cond, stmt: Stmt);
	SDoWhile(stmt: Stmt, cond: Expr);
	SFor(init: Stmt, cond: Option<Expr>, update: Option<Expr>);
	SForRange(init: Option<VarDecl>, decl: VarDecl, expr: Expr);
	SBreak;
	SContinue;
	SReturn(expr: Option<Expr>);
	SGoto(label: String);
	STry(tryBlock: Block, cases: Array<{param: Param, body: Block}>, catchAll: Option<Block>);
}