class Param of Decl, Named, Parents, Parametric {
	my rule (Maybe[Tuple[Span, Rule]])
	my body (Maybe[Body])

	on [displayName] (Str) is getter {
		return "local generic"
	}
}