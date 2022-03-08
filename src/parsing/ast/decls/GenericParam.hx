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
	final params: Null<TypeParams>;
	final parents: Parents;
	final attrs: Map<GenericParamAttr, Span>;
	final rule: Null<{span: Span, rule: GenericRule}>;
	final body: Null<DeclBody>;
}