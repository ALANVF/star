package typing;

enum GenericRule {
	Eq(l: Type, r: Type);
	Of(l: Type, r: Type);
	All(conds: List<GenericRule>);
	Any(conds: List<GenericRule>);
	One(conds: List<GenericRule>);
	Not(rule: GenericRule);
}