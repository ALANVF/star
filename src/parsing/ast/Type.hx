package parsing.ast;

import parsing.ast.TypeParams;
import text.Span;

enum TypeSeg {
	Named(_: Span, name: String, args: TypeParams);
	Blank(_: Span, args: TypeParams);
}

typedef Type = List<TypeSeg>;