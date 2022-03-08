package parsing.ast.decls;

import text.Span;

enum ProtocolAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsSealed(outsideOf: Option<Type>);
}

@:structInit
@:publicFields
class Protocol {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: Null<TypeParams>;
	final parents: Parents;
	final attrs: Map<ProtocolAttr, Span>;
	final body: DeclBody;
}