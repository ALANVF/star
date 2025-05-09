package typing;

import text.Span;

typedef LoopStart = parsing.ast.Stmt.LoopStart;
typedef LoopStop = parsing.ast.Stmt.LoopStop;

enum Stmt {
	SExpr(e: TExpr);

	SIf(
		cond: TExpr,
		then: TStmts,
		?orelse: TStmts
	);
	SCase(
		cases: Array<{cond: TExpr, then: TStmts}>,
		?orelse: TStmts
	);
	SMatch(
		value: TExpr,
		cases: Array<{pattern: Pattern, ?cond: TExpr, then: TStmts}>,
		?orelse: TStmts
	);
	SMatchAt(
		value: TExpr,
		pattern: Pattern,
		?cond: TExpr,
		then: TStmts,
		?orelse: TStmts
	);
	
	SWhile(
		cond: TExpr,
		?label: String,
		body: TStmts
	);
	SDoWhile(
		body: TStmts,
		?label: String,
		cond: TExpr
	);

	SForIn(
		lpat: Pattern,
		?lpat2: Pattern,
		inExpr: TExpr,
		?cond: TExpr,
		?label: String,
		body: TStmts
	);
	SForRange(
		?lvar: TExpr, // why is this nullable lol
		start: Tuple2<LoopStart, TExpr>,
		stop: Tuple2<LoopStop, TExpr>,
		?by: TExpr,
		?cond: TExpr,
		?label: String,
		body: TStmts
	);

	SRecurse(
		lvars: Array<TExpr>,
		?label: String,
		body: TStmts
	);

	SDo(
		?label: String,
		body: TStmts
	);

	SReturn(?value: TExpr);
	SBreak(?depth: Either<Int, String>);
	SNext(?depth: Either<Int, String>, ?lvars: Array<Tuple2<String, Local>>, ?with: Array<TExpr>);
	
	SThrow(span: Span, value: TExpr);
	STry(
		body: TStmts,
		cases: Array<{pattern: Pattern, ?cond: TExpr, then: TStmts}>,
		?orelse: TStmts
	);
}

@:publicFields @:structInit class TStmt {
	var s: Stmt;
	//var spans: Null<Map<String, Span>>;
	var orig: Null<parsing.ast.Stmt>;

	function new(s: Stmt, ?orig: parsing.ast.Stmt) {
		this.s = s;
		this.orig = orig;
	}
}