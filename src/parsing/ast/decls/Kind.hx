package parsing.ast.decls;

import text.Span;

enum KindAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
	IsFlags;
	IsStrong;
	IsUncounted;
}

@:structInit
@:publicFields
class Kind /*implements Decl.AllowsCase, Decl.AllowsMember, Decl.AllowsAnyType, Decl.AllowsMethod, Decl.Allows*/ {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final repr: Option<Type>;
	final parents: Parents;
	final attrs: Map<KindAttr, Span>;
	final body: DeclBody;
	
	/*[
		| `Module of module_decl
		| `Class of class_decl
		| `Protocol of protocol_decl
		| `Kind of kind_decl
		| `Alias of alias_decl
		| `Member of Member.t
		| `Case of Case.t
		| `Method of Method.t
		| `Base_init of Base_method.t
		| `Operator of Operator.t
		| `Deinit of Base_method.t
	] list delims_loc*/
}