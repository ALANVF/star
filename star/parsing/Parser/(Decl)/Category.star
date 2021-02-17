class Category of Namespace, Generic {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
	}

	my path (Type)
	my type (Maybe[Type])
	my attrs (Attrs)

	on [displayName] (Str) is getter {
		return "category"
	}
}