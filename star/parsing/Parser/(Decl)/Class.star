class Class of Namespace, Parents {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [is: (Span) friend: (TypeSpec)]
		has [is: (Span) sealed: (Maybe[Type])]
		has [begin: (Span) isNative: (Array[Tuple[Ident, Expr]]) end: (Span)]
		has [isStrong: (Span)]
		has [isUncounted: (Span)]
	}

	my attrs (Attrs)

	on [displayName] (Str) is getter {
		return "class"
	}
}