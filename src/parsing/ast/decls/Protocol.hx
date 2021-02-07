package parsing.ast.decls;

import text.Span;

enum ProtocolAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
}

@:structInit
@:publicFields
class Protocol {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final parents: Parents;
	final attrs: Map<ProtocolAttr, Span>;
	final body: DeclBody;
}