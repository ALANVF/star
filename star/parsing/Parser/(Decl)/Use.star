kind Use of Decl, Generic {
	has [import: (Tree) from: (Maybe[From]) as: (Maybe[Tuple[Span, Tree]])]
	has [pragma: (Ident)]

	on [displayName] (Str) is getter {
		return "import"
	}
}