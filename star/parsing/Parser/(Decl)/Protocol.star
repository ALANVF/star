class Protocol of Namespace, Parents {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
		has [is: (Span) sealed: (Maybe[Type])]
	}

	my attrs (Attrs)

	on [displayName] (Str) is getter {
		return "protocol"
	}
}