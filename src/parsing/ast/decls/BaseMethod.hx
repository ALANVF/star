package parsing.ast.decls;

import text.Span;

enum BaseMethodAttr {
	IsStatic;
}

@:structInit
@:publicFields
class BaseMethod {
	final span: Span;
	final attrs: Map<BaseMethodAttr, Span>;
	final body: Body;
}