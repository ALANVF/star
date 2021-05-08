package compiler;

@:using(compiler.ExprTools.LambdaCaptureTools)
enum LambdaCapture {
	LExpr(expr: Expr);
	LByRef;
	LByVal;
}

@:using(compiler.ExprTools)
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
	ELambda(
		captures: Array<LambdaCapture>,
		template: Option<Template>,
		params: Array<Param>,
		attrs: Attrs,
		ret: Option<Type>,
		requires: Option<Requires>,
		body: Block
	);
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
	EPrefix(op: PrefixOp, r: Expr);
	ESuffix(l: Expr, op: SuffixOp);
	EInfix(l: Expr, op: InfixOp, r: Expr);
	EIndex(expr: Expr, index: Expr);
	ECall(expr: Expr, typeArgs: Option<Array<Type>>, args: Array<Expr>);
	EDot(expr: Expr, name: String);
	EDotStatic(expr: Expr, path: TypePath, name: String);
	EDotTemplate(expr: Expr, name: String);
	EDotRef(l: Expr, r: Expr);
	EArrow(expr: Expr, name: String);
	EArrowStatic(expr: Expr, path: TypePath, name: String);
	EArrowTemplate(expr: Expr, name: String);
	EArrowRef(l: Expr, r: Expr);
	EScope(type: Type, name: String);
	ETernary(cond: Expr, yes: Expr, no: Expr);
	ESizeof(expr: Expr);
	ESizeofPack(expr: Expr);
	EAlignof(type: Type);
	ETypeid(expr: Expr);
	ERequires(r: Requires);
	EType(t: Type);
	ERaw(code: String);
}