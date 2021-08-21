package parsing.ast.decls;

import text.Span;

enum UseKind {
	Import(spec: UseTree, ?from: UseFrom, ?as: Tuple2<Span, UseTree>);
	Pragma(_: Span, pragma: String);
}

@:structInit
@:publicFields
class Use {
	final generics: List<GenericParam>;
	final span: Span;
	final kind: UseKind;
}