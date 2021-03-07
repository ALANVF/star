protocol EmptyMethod of Decl {
	kind Attrs is flags {
		has [empty]
		has [isStatic: (Span)]
	}

	my attrs (Attrs)
	my body (Block)
}