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
	final params: Null<TypeParams>;
	final repr: Null<Type>;
	final parents: Parents;
	final attrs: Map<KindAttr, Span>;
	final body: DeclBody;
}