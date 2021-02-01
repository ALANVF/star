package parsing.ast.decls;

import text.Span;

enum InitAttr {
	IsHidden(outsideOf: Option<Type>);
	IsNoinherit;
	IsUnordered;
	IsNative(sym: Option<Ident>);
	IsAsm;
}

enum InitKind {
	Single(name: Ident);
	Multi(params: Array<MultiParam>);
}

@:structInit
@:publicFields
class Init {
	final generics: List<GenericParam>;
	final span: Span;
	final spec: Delims<InitKind>;
	final attrs: Map<InitAttr, Span>;
	final body: Option<Block>;
}