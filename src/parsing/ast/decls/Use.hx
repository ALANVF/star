package parsing.ast.decls;

import text.Span;

enum UseKind {
	Import(spec: TypesSpec);
	ImportFrom(spec: TypesSpec, _: Span, from: Type);
	Pragma(_: Span, pragma: String);
}

@:structInit
@:publicFields
class Use {
	final generics: List<GenericParam>;
	final span: Span;
	final kind: UseKind;
}