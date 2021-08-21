kind Stmt {
	has [expr: (Expr)]

	has [
		my: (Span)
		name: (Ident)
		type: (Maybe[Type])
		value: (Maybe[Expr])
	]
	
	has [
		if: (Span), (Expr)
		then: (Block)
		others: (Array[OrIf])
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
		then: (Block)
		else: (Maybe[Tuple[Span, Block]])
	]

	has [
		while: (Span), (Expr)
		label: (Maybe[Tuple[Span, Ident]])
		do: (Block)
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
		do: (Block)
	]

	has [
		for: (Span)
		var: (Expr)
		start: (Loop.Start)
		stop: (Loop.Stop)
		step: (Maybe[Tuple[Span, Expr]])
		while: (Maybe[Tuple[Span, Expr]])
		label: (Maybe[Tuple[Span, Ident]])
		do: (Block)
	]

	has [do: (Span) label: (Maybe[Tuple[Span, Ident]]), (Block)]

	has [return: (Span)]
	has [return: (Span) value: (Expr)]

	has [break: (Span)]
	has [break: (Span) depth: (Span), (Int)]
	has [break: (Span) label: (Span), (Str)]

	has [next: (Span)]
	has [next: (Span) depth: (Span), (Int)]
	has [next: (Span) label: (Span), (Str)]

	has [throw: (Span), (Expr)]

	has [
		try: (Span), (Block)
		catch: (Span)
		cases: (Array[PatternAt])
		else: (Maybe[Tuple[Span, Then]])
	]
}