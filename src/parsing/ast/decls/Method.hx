package parsing.ast.decls;

import text.Span;

enum MethodAttr {
	IsStatic;
	IsHidden(outsideOf: Option<Type>);
	IsMain;
	IsGetter;
	IsSetter;
	IsNoinherit;
	IsUnordered;
	IsNative(sym: Option<Ident>);
	IsInline;
	IsAsm;
	IsMacro;
}

enum MethodKind {
	Single(name: Ident);
	Multi(params: Array<MultiParam>);
	Cast(type: Type);
}

@:structInit
@:publicFields
class Method {
	final generics: List<GenericParam>;
	final span: Span;
	final spec: Delims<MethodKind>;
	final ret: Option<Type>;
	final attrs: Map<MethodAttr, Span>;
	final body: Option<Body>;
}