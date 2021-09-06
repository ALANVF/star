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

enum Stmt {
	SExpr(expr: Expr);
	SIf(
		_: Span,
		cond: Expr,
		thenBlk: Block,
		elseBlk: Option<Tuple2<Span, Block>>
	);
	SCase(
		_begin: Span,
		cases: Array<{span: Span, cond: Expr, then: Then}>,
		otherwise: Option<Tuple2<Span, Then>>,
		_end: Span
	);
	SMatch(
		_: Span,
		value: Expr,
		_begin: Span,
		cases: Array<{span: Span, pattern: Expr, when: Option<Tuple2<Span, Expr>>, then: Then}>,
		otherwise: Option<Tuple2<Span, Then>>,
		_end: Span
	);
	SShortMatch(
		_1: Span,
		value: Expr,
		_2: Span,
		pattern: Expr,
		cond: Option<Tuple2<Span, Expr>>,
		thenBlk: Block,
		elseBlk: Option<Tuple2<Span, Block>>
	);

	SWhile(
		_: Span,
		cond: Expr,
		label: Option<Tuple2<Span, Ident>>,
		block: Block
	);
	SDoWhile(
		_1: Span,
		label: Option<Tuple2<Span, Ident>>,
		block: Block,
		_2: Span,
		cond: Expr
	);
	SForIn(
		_: Span,
		lvar: Expr,
		lvar2: Option<Expr>,
		inSpan: Span,
		inExpr: Expr,
		cond: Option<Tuple2<Span, Expr>>,
		label: Option<Tuple2<Span, Ident>>,
		block: Block
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
		step: Option<Tuple2<Span, Expr>>,
		cond: Option<Tuple2<Span, Expr>>,
		label: Option<Tuple2<Span, Ident>>,
		block: Block
	);
	SDo(_: Span, label: Option<Tuple2<Span, Ident>>, block: Block);
	SReturn(_: Span, value: Option<Expr>);
	SBreak(_: Span, depth: Option<Tuple2<Span, Either<Int, String>>>);
	SNext(_: Span, depth: Option<Tuple2<Span, Either<Int, String>>>);
	SThrow(_: Span, value: Expr);
	STry(
		_: Span,
		block: Block,
		_begin: Span,
		cases: Array<{span: Span, pattern: Expr, when: Option<Tuple2<Span, Expr>>, then: Then}>,
		otherwise: Option<Tuple2<Span, Then>>,
		_end: Span
	);
}