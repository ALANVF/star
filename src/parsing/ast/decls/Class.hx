package parsing.ast.decls;

import text.Span;

enum ClassAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsSealed(outsideOf: Option<Type>);
	IsNative(_begin: Span, spec: Array<{label: Ident, expr: Expr}>, _end: Span);
	IsStrong;
	IsUncounted;
}

@:structInit
@:publicFields
class Class {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: Null<TypeParams>;
	final parents: Parents;
	final attrs: Map<ClassAttr, Span>;
	final body: DeclBody;
}