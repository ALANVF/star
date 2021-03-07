class Member of Decl, Named {
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
	my value (Maybe[Expr])
	my attrs (Attrs)

	on [displayName] (Str) is getter {
		match attrs at Attrs[isStatic: _] & _ {
			return "static member"
		} else {
			return "instance member"
		}
	}
}