package parsing.ast.decls;

import text.Span;

typedef Parents = Option<{span: Span, parents: Array<Type>}>;