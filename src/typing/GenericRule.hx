package typing;

enum GenericRule {
	Eq(l: Type, r: Type);
	Of(l: Type, r: Type);
	All(conds: List<GenericRule>);
	Any(conds: List<GenericRule>);
	One(conds: List<GenericRule>);
	Not(rule: GenericRule);
}

@:publicFields
@:noCompletion
class Tools {
	static inline function fromAST(_: Enum<GenericRule>, lookup: ILookupType, parserRule: parsing.ast.decls.GenericRule) {
		return _fromAST(lookup, parserRule);
	}

	@:noUsing
	static function _fromAST(lookup: ILookupType, parserRule: parsing.ast.decls.GenericRule) return switch parserRule {
		case Eq(l, _, r): Eq(lookup.makeTypePath(l), lookup.makeTypePath(r));
		case Ne(l, _, r): Not(Eq(lookup.makeTypePath(l), lookup.makeTypePath(r)));
		case Of(l, _, r): Of(lookup.makeTypePath(l), lookup.makeTypePath(r));
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