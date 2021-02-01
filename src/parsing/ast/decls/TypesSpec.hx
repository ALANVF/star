package parsing.ast.decls;

import text.Span;

enum TypesSpec {
	One(type: Type);
	Many(_begin: Span, types: Array<Type>, _end: Span);
}