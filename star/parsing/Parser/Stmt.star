kind Stmt {
	has [expr: (Expr)]
	
	has [
		if: (Span), (Expr)
		then: (Then)
		else: (Maybe[Tuple[Span, Block]])
	]
	
	has [
		case: (Span)
		cases: (Array[CaseAt])
		else: (Maybe[Tuple[Span, Then]])
	]

	has [
		match: (Span)
		value: (Expr)
		cases: (Array[PatternAt])
		else: (Maybe[Tuple[Span, Then]])
	]

	has [
		match: (Span)
		value: (Expr)
		at: (Span), (Expr)
		if: (Maybe[Tuple[Span, Expr]])
		then: (Then)
		else: (Maybe[Tuple[Span, Block]])
	]

	has [
		while: (Span), (Expr)
		label: (Maybe[Tuple[Span, Ident]])
		do: (Then)
	]

	has [
		do: (Span) label: (Maybe[Tuple[Span, Ident]]), (Block)
		while: (Span), (Expr)
	]

	has [
		for: (Span)
		var: (Expr)
		var': (Maybe[Expr])
		in: (Tuple[Span, Expr])
		while: (Maybe[Tuple[Span, Expr]])
		label: (Maybe[Tuple[Span, Ident]])
		do: (Then)
	]

	has [
		for: (Span)
		var: (Expr)
		start: (Loop.Start)
		stop: (Loop.Stop)
		step: (Maybe[Tuple[Span, Expr]])
		while: (Maybe[Tuple[Span, Expr]])
		label: (Maybe[Tuple[Span, Ident]])
		do: (Then)
	]

	has [
		recurse: (Span)
		lvars: (Array[Expr]) ;-- rly it's just var decls, but we need to allow existing vars too
		label: (Maybe[Tuple[Span, Ident]])
		do: (Then)
	]

	has [do: (Span) label: (Maybe[Tuple[Span, Ident]]), (Block)]

	has [return: (Span)]
	has [return: (Span) value: (Expr)]

	has [break: (Span)]
	has [break: (Span) depth: (Span), (Int)]
	has [break: (Span) label: (Span), (Str)]

	has [next: (Span) with: (Maybe[Array[Expr]])]
	has [next: (Span) depth: (Span), (Int) with: (Maybe[Array[Expr]])]
	has [next: (Span) label: (Span), (Str) with: (Maybe[Array[Expr]])]

	has [throw: (Span), (Expr)]

	has [
		try: (Span), (Block)
		catch: (Span)
		cases: (Array[PatternAt])
		else: (Maybe[Tuple[Span, Then]])
	]
}