package compiler;

enum Expr {
	ENullptr;
	EThis;
	EBool(b: Bool);
	EInt(i: Int, exp: Option<Int>);
	EFloat(i: Int, f: String, exp: Option<Int>);
	EChar(c: Char);
	EString(s: String);
	EName(name: String);
	EParen(expr: Expr);
	EInitList(l: InitList);
	ETypeCtor(type: Type, ctor: InitCtor);
	ELambda(captures: Array<Expr>, params: Array<Param>, ret: Option<Type>, body: Block);
	ENew(
		placement: Option<Array<Expr>>,
		constraint: Option<Type>,
		type: Type,
		hasTypeParens: Bool,
		ctor: Option<InitCtor>
	);
	EDelete(isArray: Bool, expr: Expr);
	EThrow(expr: Option<Expr>);
	ECast(type: Type, expr: Expr);
	EConstCast(type: Type, expr: Expr);
	EStaticCast(type: Type, expr: Expr);
	EDynamicCast(type: Type, expr: Expr);
	EReinterpretCast(type: Type, expr: Expr);
	EPrefix(op: PrefixOp, expr: Expr);
	ESuffix(expr: Expr, op: SuffixOp);
	EIndex(expr: Expr, index: Expr);
	ECall(expr: Expr, typeArgs: Option<Array<Type>>, args: Array<Expr>);
	EDot(expr: Expr, isTemplate: Bool, isRef: Bool, name: String);
	EArrow(expr: Expr, isTemplate: Bool, isRef: Bool, name: String);
	EScope(type: Type, name: String);
	ETernary(cond: Expr, yes: Expr, no: Expr);
	ESizeofType(type: Type);
	ESizeof(expr: Expr);
	ERaw(code: String);
}