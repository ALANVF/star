package parsing.ast.decls;

import text.Span;

enum MemberAttr {
	IsStatic;
	IsHidden(outsideOf: Option<Type>);
	IsReadonly;
	IsGetter(name: Option<Ident>);
	IsSetter(name: Option<Ident>);
	IsNoinherit;
}

@:structInit
@:publicFields
class Member {
	final span: Span;
	final name: Ident;
	final type: Option<Type>;
	final attrs: Map<MemberAttr, Span>;
	final value: Option<Expr>;
}