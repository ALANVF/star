package parsing.ast.decls;

import text.Span;

enum GenericRule {
	Eq(l: Type, _: Span, r: Type);
	Ne(l: Type, _: Span, r: Type);
	Of(l: Type, _: Span, r: Type);
	And(l: GenericRule, _: Span, r: GenericRule);
	Or(l: GenericRule, _: Span, r: GenericRule);
	Xor(l: GenericRule, _: Span, r: GenericRule);
	Nor(l: GenericRule, _: Span, r: GenericRule);
	Not(_: Span, r: GenericRule);
	Paren(_begin: Span, rule: GenericRule, _end: Span);
}