package typing;

import text.Span;

typedef ImportFrom = parsing.ast.decls.UseFrom;

@:build(util.Auto.build())
class Import {
	@:ignore final generics = new MultiMap<String, Generic>();
	final span: Span;
	final spec: TypeTree;
	final from: Option<ImportFrom> = None;
	final as: Option<TypeTree> = None;
}