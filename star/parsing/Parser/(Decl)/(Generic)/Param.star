class Param of Decl, Named, Parents, Parametric {
	kind Attrs is flags {
		has [empty]
		has [begin: (Span) isNative: (Array[Tuple[Ident, Expr]]) end: (Span)]
		has [isFlags]
		has [isStrong: (Span)]
		has [isUncounted: (Span)]
	}
	
	my attrs (Attrs)
	my rule (Maybe[Tuple[Span, Rule]])
	my body (Maybe[Body])

	on [displayName] (Str) is getter {
		return "local generic"
	}
}