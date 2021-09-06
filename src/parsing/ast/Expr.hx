package parsing.ast;

import text.Span;

enum StrPart {
	PStr(str: String);
	PCode(code: Expr);
}

enum Expr {
	EName(_: Span, name: String);
	ELitsym(_: Span, name: String);
	
	ETag(_: Span, name: String, expr: Expr);

	EInt(_: Span, int: Int, exp: Option<Int>);
	EDec(_: Span, int: Int, dec: String, exp: Option<Int>);
	EChar(_: Span, char: Char);
	EStr(_: Span, parts: Array<StrPart>);
	EBool(_: Span, bool: Bool);
	EArray(_begin: Span, values: Array<Expr>, _end: Span);
	EHash(_begin: Span, pairs: Array<{k: Expr, v: Expr}>, _end: Span);
	ETuple(_begin: Span, values: Array<Expr>, _end: Span);
	EThis(_: Span);
	EWildcard(_: Span);
	EFunc(_begin: Span, params: Array<{name: Ident, type: Option<Type>}>, ret: Option<Type>, body: Array<Stmt>, _end: Span);
	EAnonArg(_: Span, depth: Int, nth: Int);
	ELiteralCtor(type: Type, literal: Expr/*EInt|EDec|EChar|EStr|EArray|EHash|ETuple|EFunc(|EBlock|ELitSym ?)*/);

	EParen(_begin: Span, exprs: Array<Expr>, _end: Span);
	EBlock(block: Block);
	
	ETypeMessage(type: Type, _begin: Span, msg: Message<Type>, _end: Span);
	ETypeCascade(type: Type, cascades: Array<Cascade<Type>>);
	ETypeMember(type: Type, member: Ident);

	EObjMessage(expr: Expr, _begin: Span, msg: Message<Expr>, _end: Span);
	EObjCascade(expr: Expr, cascades: Array<Cascade<Expr>>);
	EObjMember(expr: Expr, member: Ident);

	EPrefix(_: Span, op: Prefix, right: Expr);
	ESuffix(left: Expr, _: Span, op: Suffix);
	EInfix(left: Expr, _: Span, op: Infix, right: Expr);

	EVarDecl(_: Span, name: Ident, type: Option<Type>, value: Option<Expr>);

	EType(type: Type);
}