package parsing.ast.decls;

import text.Span;

@:structInit
@:publicFields
class GenericParam {
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final parents: Parents;
	final rule: Option<{span: Span, rule: GenericRule}>;
	final body: Option<DeclBody>;
}