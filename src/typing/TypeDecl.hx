package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build({init: false}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl
	implements IErrors
	implements ITypeDecl
{
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final generics: Array<Generic>;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	
	
	/*
	function makeGenericRule(lookup: ILookupType, parserRule: parsing.ast.decls.GenericRule): GenericRule return switch parserRule {
		case Eq(l, _, r): Eq(lookup.lookupOrAddTypePath(l), lookup.lookupOrAddTypePath(r));
		case Ne(l, _, r): Not(Eq(lookup.lookupOrAddTypePath(l), lookup.lookupOrAddTypePath(r)));
		case Of(l, _, r): Of(lookup.lookupOrAddTypePath(l), lookup.lookupOrAddTypePath(r));
		case And(l, _, r): All(switch [makeGenericRule(lookup, l), makeGenericRule(lookup, r)] {
			case [cond, All(conds)] if(r.match(And(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Or(l, _, r): Any(switch [makeGenericRule(lookup, l), makeGenericRule(lookup, r)] {
			case [cond, Any(conds)] if(r.match(Or(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Xor(l, _, r): One(switch [makeGenericRule(lookup, l), makeGenericRule(lookup, r)] {
			case [cond, One(conds)] if(r.match(Xor(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		});
		case Nor(l, _, r): Not(Any(switch [makeGenericRule(lookup, l), makeGenericRule(lookup, r)] {
			case [cond, Not(Any(conds))] if(r.match(Nor(_, _, _))): conds.prepend(cond);
			case [lcond, rcond]: List.of(lcond, rcond);
		}));
		case Not(_, rule): Not(makeGenericRule(lookup, rule));
		case Paren(_, rule, _): makeGenericRule(lookup, rule);
	}
	*/

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}
}