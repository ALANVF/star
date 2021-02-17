kind Alias of TypeDecl {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
	}

	my attrs (Attrs)

	has [opaque]
	has [opaque: (Body)]
	has [direct: (Type)]
	has [strong: (Type)]
	has [strong: (Type) body: (Body)]

	on [displayName] (Str) is getter {
		return "alias"
	}
}