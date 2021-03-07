class Operator of Decl, Generic {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [isNoinherit: (Span)]
		has [isNative: (Maybe[Ident])]
		has [isInline: (Span)]
		has [isAsm: (Span)]
		has [isMacro: (Span)]
	}

	my symbol (Ident)
	my spec (Maybe[Delims[Spec]])
	my return (Maybe[Type])
	my attrs (Attrs)
	my body (Maybe[Block])

	on [displayName] (Str) is getter {
		return "operator overload"
	}
}