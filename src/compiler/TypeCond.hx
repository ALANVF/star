package compiler;

enum TypeCond {
	CBool(b: Bool);
	CType(t: Type);
	EParen(cond: TypeCond);
	CAnd(l: TypeCond, r: TypeCond);
	COr(l: TypeCond, r: TypeCond);
	CRequires(r: Requires);
	CRaw(code: String);
}