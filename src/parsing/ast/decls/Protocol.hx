package parsing.ast.decls;

import text.Span;

enum ProtocolAttr {
	IsHidden(outsideOf: Option<Type>);
	IsFriend(spec: TypesSpec);
}

@:structInit
@:publicFields
class Protocol {
	final generics: List<GenericParam>;
	final span: Span;
	final name: Ident;
	final params: TypeParams;
	final parents: Parents;
	final attrs: Map<ProtocolAttr, Span>;
	final body: DeclBody;
	
	/*[
		| `Module of module_decl
		| `Class of class_decl
		| `Protocol of protocol_decl
		| `Kind of kind_decl
		| `Alias of alias_decl
		| `Member of Member.t
		| `Method of Method.t
		| `Operator of Operator.t
	] list delims_loc*/
}