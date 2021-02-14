package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

using typing.GenericRule.Tools;

// Dear god what have I gotten myself into

@:build(util.Auto.build())
class Generic
	implements ITypeDecl
	implements IDefaultInit
	implements IDeinit
	implements IInits
	implements IMembers
	implements IMethods
	implements IOperators
	implements IStaticDeinit
	implements IStaticInit
	implements IStaticMembers
	implements IStaticMethods
	implements ITaggedCases
	implements IValueCases
{
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var parents: Option<Array<Type>>;
	var rule: Option<GenericRule>;
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;
	final inits: Array<Init> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var staticDeinit: Option<StaticDeinit> = None;
	var staticInit: Option<StaticInit> = None;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final taggedCases: Array<TaggedCase> = [];
	final valueCases: Array<ValueCase> = [];

	static function fromAST(lookup, ast: parsing.ast.decls.GenericParam) {
		final generic = new Generic({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: ast.params.map(p -> p.of.map(lookup.makeTypePath)),
			parents: ast.parents.map(p -> p.parents.map(lookup.makeTypePath)),
			rule: ast.rule.map(r -> GenericRule.fromAST(lookup, r.rule))
		});

		return generic;
	}

	inline function declName() {
		return "generic type";
	}

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}