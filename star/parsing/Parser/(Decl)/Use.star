kind Use of Decl, Generic {
	has [import: (TypeSpec)]
	has [import: (TypeSpec) from: (Span), (Type)]
	has [pragma: (Ident)]

	on [displayName] (Str) is getter {
		return "import"
	}
}