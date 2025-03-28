package parsing.ast;

import text.Span;

enum Then {
	ThenBlock(blk: Block);
	ThenStmt(_: Span, stmt: Stmt);
}

enum LoopStart {
	LoopFrom;
	LoopAfter;
}

enum LoopStop {
	LoopTo;
	LoopUpto;
	LoopDownto;
	LoopTimes;
}

typedef MatchCase = {span: Span, pattern: Expr, when: Null<Tuple2<Span, Expr>>, then: Then}

@:using(parsing.ast.Stmt)
enum Stmt {
	SExpr(expr: Expr);
	SIf(
		_: Span,
		cond: Expr,
		then: Then,
		elseBlk: Null<Tuple2<Span, Block>>
	);
	SCase(
		_begin: Span,
		cases: Array<{span: Span, cond: Expr, then: Then}>,
		otherwise: Null<Tuple2<Span, Then>>,
		_end: Span
	);
	SMatch(
		_: Span,
		value: Expr,
		_begin: Span,
		cases: Array<MatchCase>,
		otherwise: Null<Tuple2<Span, Then>>,
		_end: Span
	);
	SShortMatch(
		_1: Span,
		value: Expr,
		_2: Span,
		pattern: Expr,
		cond: Null<Tuple2<Span, Expr>>,
		then: Then,
		elseBlk: Null<Tuple2<Span, Block>>
	);

	SWhile(
		_: Span,
		cond: Expr,
		label: Null<Tuple2<Span, Ident>>,
		body: Then
	);
	SDoWhile(
		_1: Span,
		label: Null<Tuple2<Span, Ident>>,
		block: Block,
		_2: Span,
		cond: Expr
	);
	SForIn(
		_: Span,
		lvar: Expr,
		lvar2: Null<Expr>,
		inSpan: Span,
		inExpr: Expr,
		cond: Null<Tuple2<Span, Expr>>,
		label: Null<Tuple2<Span, Ident>>,
		body: Then
	);
	SForRange(
		_: Span,
		lvar: Expr,
		startSpan: Span,
		startKind: LoopStart,
		startExpr: Expr,
		stopSpan: Span,
		stopKind: LoopStop,
		stopExpr: Expr,
		step: Null<Tuple2<Span, Expr>>,
		cond: Null<Tuple2<Span, Expr>>,
		label: Null<Tuple2<Span, Ident>>,
		body: Then
	);
	SRecurse(
		_: Span,
		lvars: Array<Expr /* rly it's just var decls, but we need to allow existing vars too */>,
		label: Null<Tuple2<Span, Ident>>,
		body: Then
	);
	SDo(_: Span, label: Null<Tuple2<Span, Ident>>, block: Block);
	SReturn(_: Span, value: Null<Expr>);
	SBreak(_: Span, depth: Null<Tuple2<Span, Either<Int, String>>>);
	SNext(_: Span, depth: Null<Tuple2<Span, Either<Int, String>>>, with: Null<Array<Expr>>);
	SThrow(_: Span, value: Expr);
	STry(
		_: Span,
		block: Block,
		_begin: Span,
		cases: Array<MatchCase>,
		otherwise: Null<Tuple2<Span, Then>>,
		_end: Span
	);
}

inline function mainSpan(self: Stmt) return self._match(
	at(SExpr(e)) => e.mainSpan(),
	at(   SIf(s, _, _, _)
		| SCase(s, _)
		| SMatch(s, _, _)
		| SShortMatch(s, _, _, _, _, _, _)
		| SWhile(s, _, _, _)
		| SDoWhile(s, _, _)
		| SForIn(s, _, _, _, _, _, _)
		| SForRange(s, _, _, _, _, _, _, _, _, _, _)
		| SRecurse(s, _, _)
		| SDo(s, _, _)
		| SReturn(s, _)
		| SBreak(s, _)
		| SNext(s, _)
		| SThrow(s, _)
		| STry(s, _, _, _, _, _)
	) => s
);