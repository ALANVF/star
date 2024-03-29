package parsing.ast.decls;

import text.Span;

enum OperatorAttr {
	IsHidden(outsideOf: Option<Type>);
	IsNoinherit;
	IsNative(sym: Option<Ident>);
	IsInline;
	IsAsm;
	IsMacro;
}

@:structInit
@:publicFields
class Operator {
	final generics: List<GenericParam>;
	final span: Span;
	final symbolSpan: Span;
	final symbol: String;
	final spec: Null<Delims<{name: Ident, type: Type}>>;
	final ret: Null<Type>;
	final attrs: Map<OperatorAttr, Span>;
	final body: Null<Body>;
}