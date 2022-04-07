kind Use of Decl, Generic {
	has [import: (Tree) from: (Maybe[From]) as: (Maybe[Tuple[Span, Tree]])]
	has [pragma: (Ident)]

	on [displayName] (Str) is getter {
		match this {
			at This[import: _ from: _ as: _] => return "import"
			at This[pragma: _] => return "pragma"
		}
	}
}