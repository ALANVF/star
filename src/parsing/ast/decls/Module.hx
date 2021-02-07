package parsing.ast.decls;

import text.Span;

enum ModuleAttr {
	IsHidden(outsideOf: Option<Type>);
	IsMain;
	IsFriend(spec: TypesSpec);
	IsNative(_: Span, lib: String);
}

@:structInit
@:publicFields
class Module {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final parents: Parents;
	final attrs: Map<ModuleAttr, Span>;
	final body: DeclBody;
}