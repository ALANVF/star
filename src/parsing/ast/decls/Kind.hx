package parsing.ast.decls;

import text.Span;

enum KindAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsSealed(outsideOf: Option<Type>);
	IsFlags;
	IsStrong;
	IsUncounted;
}

@:structInit
@:publicFields
class Kind {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: Option<TypeParams>;
	final repr: Option<Type>;
	final parents: Parents;
	final attrs: Map<KindAttr, Span>;
	final body: DeclBody;
}