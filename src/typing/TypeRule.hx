package typing;

import typing.Traits;
import typing.Effects;

@:using(typing.TypeRule)
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

@:noUsing
inline function recEq(lookup: ITypeLookup, l1, chain) return switch chain {
	case Nil2: Nil;
	case Cons2(_, r, Nil2): Cons(Eq(l1, lookup.makeTypePath(r)), Nil);
	case Cons2(_, l2, tl):
		final l2_ = lookup.makeTypePath(l2);
		Cons(Eq(l1, l2_), recEq(lookup, l2_, tl));
}

@:noUsing
inline function recOf(lookup: ITypeLookup, l1, chain) return switch chain {
	case Nil2: Nil;
	case Cons2(_, r, Nil2): Cons(Of(l1, lookup.makeTypePath(r)), Nil);
	case Cons2(_, l2, tl):
		final l2_ = lookup.makeTypePath(l2);
		Cons(Eq(l1, l2_), recOf(lookup, l2_, tl));
}

@:noUsing
inline function recCmp(lookup: ITypeLookup, l, chain) return switch chain {
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

@:noUsing
function _fromAST(lookup: ITypeLookup, parserRule: parsing.ast.decls.GenericRule) return switch parserRule {
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


function isNative(self: TypeRule, kind: NativeKind, tvar: TypeVar) return self._match(
	at(Eq(left, right)) => {
		if(left == tvar.thisType) {
			right.isNative(kind);
		} else if(right == tvar.thisType) {
			left.isNative(kind);
		} else {
			throw "todo";
		}
	},
	at(Of(left, right)) => {
		if(left == tvar.thisType) {
			right.isNative(kind);
		} else if(right == tvar.thisType) {
			left.isNative(kind);
		} else {
			throw "todo";
		}
	},

	at(All(conds)) => conds.every(cond -> isNative(cond, kind, tvar)),
	at(Any(conds)) => conds.some(cond -> isNative(cond, kind, tvar)),

	at(Not(rule)) => !isNative(rule, kind, tvar),

	_ => throw "todo "+self+" "+kind+" "+tvar.fullName()
);


function hasParentDecl(self: TypeRule, decl: TypeDecl, tvar: TypeVar) return self._match(
	at(Eq(left, right)) => {
		if(left == tvar.thisType) {
			decl.strictUnifyWithType(right) != null;
		} else if(right == tvar.thisType) {
			left.strictUnifyWithType(decl.thisType) != null;
		} else {
			throw "todo";
		}
	},
	at(Of(left, right)) => {
		if(left == tvar.thisType) {
			decl.hasParentType(right);
		} else if(right == tvar.thisType) {
			left.hasParentDecl(decl);
		} else {
			throw "todo";
		}
	},

	at(All(conds)) => conds.every(cond -> hasParentDecl(cond, decl, tvar)),
	at(Any(conds)) => conds.some(cond -> hasParentDecl(cond, decl, tvar)),

	at(Not(rule)) => !hasParentDecl(rule, decl, tvar),

	_ => throw "todo "+self+" "+decl.fullName()+" "+tvar.fullName()
);

function hasChildDecl(self: TypeRule, decl: TypeDecl, tvar: TypeVar) {
	return hasParentDecl(self, decl, tvar);
}


// TODO
function trackEffectsIn(self: TypeRule, ctx: Ctx) return self._match(
	at(Exists(t)) => {
		t.trackEffectsIn(ctx);
	},
	_ => null
);


function evalWithType(self: TypeRule, type: Type, tvar: TypeVar) return self._match(
	at(Eq(left, right)) => {
		if(left == tvar.thisType) {
			type.strictUnifyWithType(right) != null;
		} else if(right == tvar.thisType) {
			left.strictUnifyWithType(type) != null;
		} else {
			throw "todo";
		}
	},
	at(Of(left, right)) => {
		if(left == tvar.thisType) {
			type.hasParentType(right);
		} else if(right == tvar.thisType) {
			left.hasParentType(type);
		} else {
			throw "todo";
		}
	},

	at(All(conds)) => conds.every(cond -> evalWithType(cond, type, tvar)),
	at(Any(conds)) => conds.some(cond -> evalWithType(cond, type, tvar)),

	at(Not(rule)) => !evalWithType(rule, type, tvar),

	at(Exists(t)) => {
		final ctx: Ctx = {
			where: WTypevars([]),
			thisType: tvar.thisType // pretend that this works
		};
		t.trackEffectsIn(ctx)._andOr(effects => {
			trace(tvar, t, effects);
			true;
		}, {
			false;
		});
	},

	_ => throw "todo "+self+" "+type.fullName()+" "+tvar.fullName()
);


function findCast(self: TypeRule, tvar: TypeVar,
ctx: Ctx, target: Type, from: AnyTypeDecl, cache: TypeCache = Nil): Array<CastKind> return self._match(
	at(Eq(left, right)) => {
		if(left == tvar.thisType) {
			right.findCast(ctx, target, from, cache);
		} else if(right == tvar.thisType) {
			left.findCast(ctx, target, from, cache);
		} else {
			throw "todo";
		}
	},
	at(Of(left, right)) => {
		if(left == tvar.thisType) {
			right.findCast(ctx, target, from, cache);
		} else if(right == tvar.thisType) {
			left.findCast(ctx, target, from, cache);
		} else {
			throw "todo";
		}
	},

	at(All(conds)) => {
		var found = [];
		for(cond in conds.toArray()) {
			cond.findCast(tvar, ctx, target, from, cache)._match(
				at([]) => return [],
				at(kinds) => found = found.concat(kinds)
			);
		}
		found.unique();
	},
	at(Any(conds)) => conds.toArray().flatMap(cond -> cond.findCast(tvar, ctx, target, from, cache)).unique(),

	at(Not(rule)) => [], // TODO

	at(Exists(t)) => {
		t.trackEffectsIn(ctx)._and(effects => {
			trace(tvar, t, effects);
			[];
		});
	},

	_ => throw "todo "+self+" "+target.fullName()+" "+tvar.fullName()
);


function findBinaryOp(self: TypeRule, tvar: TypeVar,
ctx: Ctx, op: BinaryOp, from: Type, cache: TypeCache = Nil): Array<BinaryOpKind> return self._match(
	at(Eq(left, right)) => {
		if(left == tvar.thisType) {
			right.findBinaryOp(ctx, op, from, cache);
		} else if(right == tvar.thisType) {
			left.findBinaryOp(ctx, op, from, cache);
		} else {
			throw "todo";
		}
	},
	at(Of(left, right)) => {
		if(left == tvar.thisType) {
			right.findBinaryOp(ctx, op, from, cache);
		} else if(right == tvar.thisType) {
			left.findBinaryOp(ctx, op, from, cache);
		} else {
			throw "todo";
		}
	},

	at(All(conds)) => {
		var found = [];
		for(cond in conds.toArray()) {
			cond.findBinaryOp(tvar, ctx, op, from, cache)._match(
				at([]) => return [],
				at(kinds) => found = found.concat(kinds)
			);
		}
		found.unique();
	},
	at(Any(conds)) => conds.toArray().flatMap(cond -> cond.findBinaryOp(tvar, ctx, op, from, cache)).unique(),

	at(Not(rule)) => [], // TODO

	_ => throw "todo "+self+" "+op.symbol()+" "+tvar.fullName()
);


@:publicFields
class TypeRuleTools {
	static inline function fromAST(_: Enum<TypeRule>, lookup: ITypeLookup, parserRule: parsing.ast.decls.GenericRule) {
		return _fromAST(lookup, parserRule);
	}
}