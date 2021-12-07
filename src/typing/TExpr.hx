package typing;

import typing.BinaryOpKind;
import typing.SingleStaticKind;
import typing.SingleInstKind;
import text.Span;

typedef Prefix = parsing.ast.Prefix;
typedef Suffix = parsing.ast.Suffix;
typedef Infix = parsing.ast.Infix;
typedef AssignInfix = parsing.ast.Infix.Assignable;

enum StrPart {
	PStr(str: String);
	PCode(code: TExpr);
}

enum Expr {
	EName(name: String, loc: Local);

	ETag(tag: String, expr: TExpr);

	EInt(int: Int, ?exp: Int);
	EDec(int: Int, dec: String, ?exp: Int);
	EChar(char: Char);
	EStr(parts: Array<StrPart>);
	EBool(bool: Bool);
	EArray(values: TExprs);
	EHash(pairs: Array<Tuple2<TExpr, TExpr>>);
	ETuple(values: TExprs);
	EThis;
	EWildcard;
	EFunc(params: Array<{name: String, ?type: Type}>, ?ret: Type, body: TStmts);
	EAnonArg(depth: Int, nth: Int);
	ELiteralCtor(type: Type, literal: TExpr);

	EParen(exprs: TExprs);
	EBlock(stmts: TStmts);

	ETypeMessage(type: Type, msg: TypeMessage);
	ETypeCascade(type: Type, cascades: Array<Cascade<Type>>); // TODO: make specialized ver
	ETypeMember(type: Type, kind: SingleStaticKind);

	EObjMessage(expr: TExpr, msg: ObjMessage);
	EObjCascade(expr: TExpr, cascades: Array<ObjCascade>);
	EObjLazyMember(expr: TExpr, member: String);
	EObjMember(expr: TExpr, kind: SingleInstKind);

	EPrefix(kind: UnaryOpKind, right: TExpr);
		ELazyPrefix(op: Prefix, right: TExpr);
	ESuffix(left: TExpr, kind: UnaryOpKind);
		ELazySuffix(left: TExpr, op: Suffix);
	EInfix(left: TExpr, kinds: Array<BinaryOpKind>, right: TExpr);
		ELazyInfix(left: TExpr, op: Infix, right: TExpr);
	EInfixChain(left: TExpr, chain: List2</*kinds:*/ Array<BinaryOpKind>, /*right:*/ TExpr>);

	EVarDecl(name: String, ?type: Type, ?value: TExpr);

	// Assignment
	ESetName(name: String, loc: Local, value: TExpr);

	// From tags
	EInitThis(type: Type, msg: TypeMessage);
	EInline(expr: TExpr);
	EKindId(expr: TExpr);
	EKindSlot(expr: TExpr, i: Int);

	// Macros
	//EMacroQuote(expr: TExpr);
	//EMacroExpand(expr: TExpr);

	// Misc
	EInvalid;

	// TEMP
	EPatternType(type: Type);
}

@:publicFields @:structInit class TExpr {
	var e: Expr;
	var t: Null<Type>;
	var orig: Null<parsing.ast.Expr>;

	function new(e: Expr, ?t: Type, ?orig: parsing.ast.Expr) {
		this.e = e;
		this.t = t;
		this.orig = orig;
	}
}