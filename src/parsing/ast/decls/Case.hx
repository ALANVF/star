package parsing.ast.decls;

import text.Span;

enum Tag {
	Single(name: Ident);
	Multi(params: Array<MultiParam>);
}

enum CaseKind {
	Scalar(name: Ident, value: Null<Expr>);
	Tagged(tag: Delims<Tag>, assoc: Null<Message<Type>>);
}

@:structInit
@:publicFields
class Case {
	final span: Span;
	final kind: CaseKind;
	final init: Null<Block>; // Body currently not allowed due to collision with case alias/assoc syntax
}