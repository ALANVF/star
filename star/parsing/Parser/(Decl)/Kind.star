class Kind of Namespace, Parents {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
		has [is: (Span) sealed: (Maybe[Type])]
		has [isFlags: (Span)]
		has [isStrong: (Span)]
		has [isUncounted: (Span)]
	}

	my repr (Maybe[Type])
	my attrs (Attrs)

	on [displayName] (Str) is getter {
		return "kind"
	}
}