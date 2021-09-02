package typing;

import typing.Traits;

enum TypeRule {
	Eq(l: Type, r: Type);
	Of(l: Type, r: Type);
	Lt(l: Type, r: Type);
	Le(l: Type, r: Type);
	All(conds: List<TypeRule>);
	Any(conds: List<TypeRule>);
	One(conds: List<TypeRule>);
	Not(rule: TypeRule);
	Negate(t: Type);
	Exists(t: Type);
}

inline function recEq(lookup: ILookupType, l1, chain) return switch chain {
	case Nil2: Nil;
	case Cons2(_, r, Nil2): Cons(Eq(l1, lookup.makeTypePath(r)), Nil);
	case Cons2(_, l2, tl):
		final l2_ = lookup.makeTypePath(l2);
		Cons(Eq(l1, l2_), recEq(lookup, l2_, tl));
}

inline function recOf(lookup: ILookupType, l1, chain) return switch chain {
	case Nil2: Nil;
	case Cons2(_, r, Nil2): Cons(Of(l1, lookup.makeTypePath(r)), Nil);
	case Cons2(_, l2, tl):
		final l2_ = lookup.makeTypePath(l2);
		Cons(Eq(l1, l2_), recOf(lookup, l2_, tl));
}

inline function recCmp(lookup: ILookupType, l, chain) return switch chain {
	case Nil3: Nil;
	case Cons3(_, op, r, tl):
		final r_ = lookup.makeTypePath(r);
		Cons(switch op {
			case parsing.ast.decls.GenericRule.CmpOp.Gt: Lt(r_, l);
			case parsing.ast.decls.GenericRule.CmpOp.Ge: Le(r_, l);
			case parsing.ast.decls.GenericRule.CmpOp.Lt: Lt(l, r_);
			case parsing.ast.decls.GenericRule.CmpOp.Le: Le(l, r_);
		}, recCmp(lookup, r_, tl));
}

@:publicFields
class TypeRuleTools {
	static inline function fromAST(_: Enum<TypeRule>, lookup: ILookupType, parserRule: parsing.ast.decls.GenericRule) {
		return _fromAST(lookup, parserRule);
	}

	@:noUsing
	static function _fromAST(lookup: ILookupType, parserRule: parsing.ast.decls.GenericRule) return switch parserRule {
		case Negate(_, t): Negate(lookup.makeTypePath(t));
		case Exists(t, _): Exists(lookup.makeTypePath(t));
		
		case Eq(l, Cons2(_, r, Nil2)): Eq(lookup.makeTypePath(l), lookup.makeTypePath(r));
		case Eq(l, chain): All(recEq(lookup, lookup.makeTypePath(l), chain));
		
		case Ne(l, Cons2(_, r, Nil2)): Not(Eq(lookup.makeTypePath(l), lookup.makeTypePath(r)));
		case Ne(l, chain): Not(Any(recEq(lookup, lookup.makeTypePath(l), chain)));
		
		case Of(l, Cons2(_, r, Nil2)): Of(lookup.makeTypePath(l), lookup.makeTypePath(r));
		case Of(l, chain): All(recOf(lookup, lookup.makeTypePath(l), chain));
		
		case Cmp(l, chain): All(recCmp(lookup, lookup.makeTypePath(l), chain));
		
		case And(l, _, r): All(switch [_fromAST(lookup, l), _fromAST(lookup, r)] {
			case [cond, All(conds)] if(r.match(And(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Or(l, _, r): Any(switch [_fromAST(lookup, l), _fromAST(lookup, r)] {
			case [cond, Any(conds)] if(r.match(Or(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Xor(l, _, r): One(switch [_fromAST(lookup, l), _fromAST(lookup, r)] {
			case [cond, One(conds)] if(r.match(Xor(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Nor(l, _, r): Not(Any(switch [_fromAST(lookup, l), _fromAST(lookup, r)] {
			case [cond, Not(Any(conds))] if(r.match(Nor(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		}));
		
		case Not(_, rule): Not(_fromAST(lookup, rule));
		case Paren(_, rule, _): _fromAST(lookup, rule);
	}
}