package typing;

import text.Span;

@:build(util.Auto.build())
class Import {
	final generics: Array<Generic> = [];
	final span: Span;
	final imports: Array<TypePath>;
	final from: Option<TypePath> = None;
}