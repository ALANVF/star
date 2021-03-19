class Method of Decl, Generic {
	kind Attrs is flags {
		has [empty]
		has [is: (Span) hidden: (Maybe[Type])]
		has [isMain: (Span)]
		has [isGetter: (Span)]
		has [isSetter: (Span)]
		has [isNoinherit: (Span)]
		has [isUnordered: (Span)]
		has [is: (Span) native: (Maybe[Ident])]
		has [isInline: (Span)]
		has [isAsm: (Span)]
		has [isMacro: (Span)]
	}

	my spec (Delims[Spec])
	my return (Maybe[Type])
	my attrs (Attrs)
	my body (Maybe[Block])

	on [displayName] (Str) is getter {
		match attrs at Attrs[isStatic: _] & _ {
			return "static method"
		} else {
			return "instance method"
		}
	}
}