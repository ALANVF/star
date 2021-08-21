package parsing.ast.decls;

import text.Span;

enum UseFrom {
	UType(_: Span, t: parsing.ast.Type);
	UFile(_1: Span, _2: Span, file: String);
}