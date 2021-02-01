package parsing.ast.decls;

import text.Span;

enum CategoryAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
}

@:structInit
@:publicFields
class Category {
	final generics: List<GenericParam>;
	final span: Span;
	final path: Type;
	final type: Option<Type>;
	final attrs: Map<CategoryAttr, Span>;
	final body: DeclBody;
}