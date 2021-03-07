class Init of Decl, Generic {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [isNoinherit: (Span)]
		has [isUnordered: (Span)]
		has [is: (Span) native: (Maybe[Ident])]
		has [isAsm: (Span)]
	}

	my spec (Delims[Spec])
	my attrs (Attrs)
	my body (Maybe[Block])

	on [displayName] (Str) is getter {
		return "initializer"
	}
}