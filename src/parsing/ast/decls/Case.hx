package parsing.ast.decls;

import text.Span;

enum Tag {
	Single(name: Ident);
	Multi(params: Array<{label: Option<Ident>, name: Option<Ident>, type: Type}>);
}

enum CaseKind {
	Scalar(name: Ident, value: Option<Expr>);
	Tagged(tag: Delims<Tag>, assoc: Option<Message<Type>>);
}

@:structInit
@:publicFields
class Case {
	final span: Span;
	final kind: CaseKind;
	final init: Option<Block>;
}