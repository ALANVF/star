package compiler;

enum RequiresStmt {
	RStmt(s: Stmt);
	RReq(e: Expr);
	RType(t: Type);
	RNested(r: TypeCond);
}

typedef RequiresBody = Array<RequiresStmt>;

@:build(util.Auto.build())
class Requires {
	var args: Array<Param>;
	var body: RequiresBody;
}