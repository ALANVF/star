package parsing.ast.decls;

import text.Span;

enum AliasAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
}

enum AliasKind {
	Opaque(body: Option<DeclBody>);
	Direct(_: Span, type: Type);
	Strong(type: Type, body: Option<DeclBody>);
}

@:structInit
@:publicFields
class Alias {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final kind: AliasKind;
	final attrs: Map<AliasAttr, Span>;
}