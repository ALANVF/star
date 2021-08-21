package parsing.ast.decls;

import text.Span;
import parsing.ast.Type;

enum UseTree {
	UType(t: Type);
	UTypes(types: Array<Type>);
	UMap(m: Array<Tuple3<Type, Span, UseTree>>);
}