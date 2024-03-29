package parsing.ast.decls;

import text.Span;

enum ModuleAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsSealed(outsideOf: Option<Type>);
	IsMain;
	IsNative(_: Span, lib: String);
}

@:structInit
@:publicFields
class Module {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: Null<TypeParams>;
	final parents: Parents;
	final attrs: Map<ModuleAttr, Span>;
	final body: DeclBody;
}