package parsing.ast.decls;

import text.Span;

enum GenericParamAttr {
	IsNative(_begin: Span, spec: Array<{label: Ident, expr: Expr}>, _end: Span);
	IsFlags;
	IsStrong;
	IsUncounted;
}

@:structInit
@:publicFields
class GenericParam {
	final span: Span;
	final name: Ident;
	final params: Option<TypeParams>;
	final parents: Parents;
	final attrs: Map<GenericParamAttr, Span>;
	final rule: Option<{span: Span, rule: GenericRule}>;
	final body: Option<DeclBody>;
}