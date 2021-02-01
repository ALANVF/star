package parsing.ast;

import text.Span;

enum Then {
	ThenBlock(blk: Block);
	ThenStmt(_: Span, stmt: Stmt);
}

enum LoopVar {
	LDecl(_: Span, name: Ident, type: Option<Type>);
	LVar(name: Ident);
	LIgnore(_: Span);
}

enum LoopStart {
	LoopFrom;
	LoopAfter;
}

enum LoopStop {
	LoopTo;
	LoopUpto;
	LoopDownto;
}

enum Stmt {
	SExpr(expr: Expr);
	SVarDecl(
		_: Span,
		name: Ident,
		type: Option<Type>,
		value: Option<Expr>
	);
	SIf(
		_: Span,
		cond: Expr,
		thenBlk: Block,
		others: Array<{span: Span, cond: Expr, blk: Block}>,
		elseBlk: Option<{span: Span, blk: Block}>
	);
	SCase(
		_begin: Span,
		cases: Array<{span: Span, cond: Expr, then: Then}>,
		otherwise: Option<{span: Span, then: Then}>,
		_end: Span
	);
	SMatch(
		_: Span,
		value: Expr,
		_begin: Span,
		cases: Array<{span: Span, pattern: Expr, when: Option<{span: Span, cond: Expr}>, then: Then}>,
		otherwise: Option<{span: Span, then: Then}>,
		_end: Span
	);
	SShortMatch(
		_1: Span,
		value: Expr,
		_2: Span,
		pattern: Expr,
		cond: Option<{span: Span, cond: Expr}>,
		thenBlk: Block,
		elseBlk: Option<{span: Span, blk: Block}>
	);

	SWhile(
		_: Span,
		cond: Expr,
		block: Block
	);
	SDoWhile(
		_1: Span,
		block: Block,
		_2: Span,
		cond: Expr
	);
	SForIn(
		_: Span,
		lvar: LoopVar,
		lvar2: Option<LoopVar>,
		inSpan: Span,
		inExpr: Expr,
		cond: Option<{span: Span, expr: Expr}>,
		block: Block
	);
	SForRange(
		_: Span,
		lvar: LoopVar,
		startSpan: Span,
		startKind: LoopStart,
		startExpr: Expr,
		stopSpan: Span,
		stopKind: LoopStop,
		stopExpr: Expr,
		step: Option<{span: Span, expr: Expr}>,
		cond: Option<{span: Span, expr: Expr}>,
		block: Block
	);
	SDo(_: Span, block: Block);
	SReturn(_: Span, value: Option<Expr>);
	SBreak(_: Span, depth: Option<{span: Span, depth: Int}>);
	SNext(_: Span, depth: Option<{span: Span, depth: Int}>);
	SThrow(_: Span, value: Expr);
	STry(
		_: Span,
		block: Block,
		_begin: Span,
		cases: Array<{span: Span, pattern: Expr, when: Option<{span: Span, cond: Expr}>, then: Then}>,
		otherwise: Option<{span: Span, then: Then}>,
		_end: Span
	);
}