package parsing.ast;

import parsing.ast.TypeParams;
import text.Span;

enum TypeSeg {
	Named(_: Span, name: String, args: TypeParams);
	Blank(_: Span, args: TypeParams);
}

@:structInit
@:publicFields
class Type {
	final path: List<TypeSeg>;

	function new(path) {
		this.path = path;
	}
}