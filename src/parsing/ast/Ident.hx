package parsing.ast;

import text.Span;

//@:struct
@:structInit
@:publicFields
class Ident {
	final span: Span;
	final name: String;

	function new(span, name) {
		this.span = span;
		this.name = name;
	}
}