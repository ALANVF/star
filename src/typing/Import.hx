package typing;

import text.Span;

typedef ImportFrom = parsing.ast.decls.UseFrom;

@:build(util.Auto.build())
class Import {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final span: Span;
	final spec: TypeTree;
	final from: Option<ImportFrom> = None;
	final as: Option<TypeTree> = None;
}