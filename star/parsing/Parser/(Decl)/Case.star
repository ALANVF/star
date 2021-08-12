kind Case of Decl {
	kind Tag {
		has [single: (Ident)]
		has [multi: (Multi.Params)]
	}

	my init (Maybe[Block]) ;-- Stmt.Body currently not allowed due to collision with case alias/assoc syntax

	has [name: (Ident) value: (Maybe[Expr])]
	has [tag: (Delims[Tag]) assoc: (Maybe[Message[Type]])]

	on [displayName] (Str) is getter {
		return "case"
	}
}