package parsing.ast.decls;

import text.Span;

enum ClassAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsNative(_begin: Span, spec: Array<{label: Ident, expr: Expr}>, _end: Span);
	IsStrong;
	IsUncounted;
}

@:structInit
@:publicFields
class Class {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final parents: Parents;
	final attrs: Map<ClassAttr, Span>;
	final body: DeclBody;
	
	/*[
		| `Module of module_decl
		| `Class of class_decl
		| `Protocol of protocol_decl
		| `Kind of kind_decl
		| `Alias of alias_decl
		| `Member of Member.t
		| `Method of Method.t
		| `Init of Init.t
		| `Base_init of Base_method.t
		| `Operator of Operator.t
		| `Deinit of Base_method.t
	] list delims_loc*/
}