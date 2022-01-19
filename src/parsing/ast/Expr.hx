package parsing.ast;

import text.Span;

enum StrPart {
	PStr(str: String);
	PCode(code: Expr);
}

@:using(parsing.ast.Expr)
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
	EAnonFunc(depth: Int, nparams: Int, types: Null<Map<Int, Type>>, expr: Expr);
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


function mainSpan(self: Expr) return self._match(
	at(EName(s, _) | ELitsym(s, _) | ETag(s, _, _)
	| EInt(s, _, _)
	| EDec(s, _, _, _)
	| EChar(s, _)
	| EStr(s, _)
	| EBool(s, _)
	| EThis(s)
	| EWildcard(s)
	| EAnonArg(s, _, _)
	| EPrefix(s, _, _)
	| ESuffix(_, s, _)
	| EInfix(_, s, _, _)) => s,

	at(EArray(b, _, e)
	| EHash(b, _, e)
	| ETuple(b, _, e)
	| EFunc(b, _, _, _, e)
	| EParen(b, _, e)
	| EBlock({begin: b, end: e})
	| ETypeMessage(_, b, _, e)
	| EObjMessage(_, b, _, e)) => b.union(e),

	at(EAnonFunc(_, _, _, e)) => e.mainSpan(),

	at(ELiteralCtor(t, c)) => t.span().union(c.mainSpan()),

	at(ETypeCascade(t, _)) => t.span(),
	at(EObjCascade(o, _)) => o.mainSpan(),

	at(ETypeMember(t, m)) => t.span().union(m.span),
	at(EObjMember(o, m)) => o.mainSpan().union(m.span),

	at(EVarDecl(s, n, _, _)) => s.union(n.span),

	at(EType(t)) => t.span()
);