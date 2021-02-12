package typing;

import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build({init: false}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl /*implements ILookupType*/ {
	final file: File;
	final generics: Array<Generic>;
	final span: Span;
	final name: Ident;
	final params: Option<Array<Type>>;
	final hidden: Option<Option<Type>>;
	final friends: Array<Type>;
	

	
	
	/*
	abstract function lookupType(span: Option<Span>, name: String, params: Option<Array<Type>>): Option<Type>;
	
	abstract function addType(span: Option<Span>, name: String, params: Option<Array<Type>>, type: Type): Type;

	abstract function lookupOrAddType(span: Option<Span>, name: String, params: Option<Array<Type>>): Type;

	function lookupTypePath(path: TypePath): Option<Type> {
		return Util.match(path,
			at([]) => throw "error!",
			at([Blank(_, None), ..._]) => throw "NYI!",
			at([Blank(_, Some(_)), ..._]) => throw "error!",
			at([Named(span, name, params), ...rest]) => {
				final params2 = params.map(d -> d.of.map(this.lookupOrAddTypePath));
				switch this.lookupType(Some(span), name, params2) {
					case None: None;
					case Some(type): type.lookupTypePath(rest);
				}
			}
		);
	}
	
	function addTypePath(path: TypePath, type: Type): Type {
		throw "todo!";
	}
	
	function lookupOrAddTypePath(path: TypePath): Type {
		throw "todo!";
	}
	
	
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
}