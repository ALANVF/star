package typing;

import text.Span;

typedef ImportFrom = parsing.ast.decls.UseFrom;

@:publicFields
@:structInit
class Import {
	final typevars = new MultiMap<String, TypeVar>();
	final span: Span;
	final spec: TypeTree;
	final from: Option<ImportFrom> = None;
	final as: Option<TypeTree> = None;
}