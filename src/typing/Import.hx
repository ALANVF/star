package typing;

import text.Span;

@:build(util.Auto.build())
class Import {
	@:ignore final generics = new MultiMap<String, Generic>();
	final span: Span;
	final imports: Array<TypePath>;
	final from: Option<TypePath> = None;
}