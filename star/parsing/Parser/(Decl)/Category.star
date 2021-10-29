class Category of Decl, Generic;[, Parametric] {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
	}

	my path (Type)
	my type (Maybe[Type])
	my attrs (Attrs)
	my body (Body)

	on [displayName] (Str) is getter {
		return "category"
	}
}