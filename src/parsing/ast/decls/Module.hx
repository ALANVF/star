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
	
	/*[
		| `Module of module_decl
		| `Class of class_decl
		| `Protocol of protocol_decl
		| `Kind of kind_decl
		| `Alias of alias_decl
		| `Member of Member.t
		| `Method of Method.t
		| `Base_init of Base_method.t
		| `Deinit of Base_method.t
	] list delims_loc*/
}