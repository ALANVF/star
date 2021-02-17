kind Case of Decl {
	kind Tag {
		has [single: name (Ident)]
		has [multi: params (Array[Maybe[Ident], Maybe[Ident], Type])]
	}

	my init (Maybe[Block])

	has [name: (Ident) value: (Maybe[Expr])]
	has [tag: (Delims[Tag])]

	on [displayName] (Str) is getter {
		return "case"
	}
}