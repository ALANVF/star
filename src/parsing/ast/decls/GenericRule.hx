package parsing.ast.decls;

import text.Span;

enum CmpOp {
	Lt;
	Le;
	Gt;
	Ge;
}

enum GenericRule {
	Negate(_: Span, r: Type);
	Exists(l: Type, _: Span);
	
	Eq(l: Type, chain: List2<Span, Type>);
	Ne(l: Type, chain: List2<Span, Type>);
	Of(l: Type, chain: List2<Span, Type>);
	
	Cmp(l: Type, chain: List3<Span, CmpOp, Type>);
	
	And(l: GenericRule, _: Span, r: GenericRule);
	Or(l: GenericRule, _: Span, r: GenericRule);
	Xor(l: GenericRule, _: Span, r: GenericRule);
	Nor(l: GenericRule, _: Span, r: GenericRule);
	
	Not(_: Span, r: GenericRule);
	Paren(_begin: Span, rule: GenericRule, _end: Span);
}