kind Alias of TypeDecl {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
		has [isNoinherit: (Span)]
	}

	my attrs (Attrs)

	has [opaque: (Maybe[Body])]
	has [direct: (Type)]
	has [strong: (Type) body: (Maybe[Body])]

	on [displayName] (Str) is getter {
		return "alias"
	}
}