class Module of Namespace, Parents {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
		has [isMain: (Span)]
		has [is: (Span) native: (Ident)]
	}

	my attrs (Attrs)

	on [displayName] (Str) is getter {
		return "module"
	}
}