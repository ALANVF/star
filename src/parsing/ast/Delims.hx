package parsing.ast;

import text.Span;

typedef Delims<T> = {
	begin: Span,
	of: T,
	end: Span
}