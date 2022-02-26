class Member of NamedDecl {
	kind Attrs is flags {
		has [empty]
		has [isStatic: (Span)]
		has [is: (Span) hidden: (Maybe[Type])]
		has [isReadonly: (Span)]
		has [is: (Span) getter: (Maybe[Ident])]
		has [is: (Span) setter: (Maybe[Ident])]
		has [isNoinherit: (Span)]
	}

	my type (Maybe[Type])
	my attrs (Attrs)
	my value (Maybe[Expr])

	on [displayName] (Str) is getter {
		match attrs at Attrs[isStatic: _] & _ {
			return "static member"
		} else {
			return "instance member"
		}
	}
}