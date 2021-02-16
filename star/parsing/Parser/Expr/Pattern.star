kind Pattern of Expr {
	has [
		capture: (Span)
		name: (Ident)
		type: (Maybe[Type])
		value: (Maybe[This])
	]
}