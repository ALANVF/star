kind Stmt {
	has [expr: (Expr)]

	has [
		my: (Span)
		name: (Ident)
		type: (Maybe[Type])
		value: (Option[Expr])
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
		at: (Span), (Expr.Pattern)
		if: (Maybe[Tuple[Span, Expr]])
		then: (Block)
		else: (Maybe[Tuple[Span, Block]])
	]

	has [
		while: (Span), (Expr)
		do: (Block)
	]

	has [
		do: (Span), (Block)
		while: (Span), (Expr)
	]

	has [
		for: (Span)
		var: (Loop.Var)
		var': (Maybe[Loop.Var])
		in: (Tuple[Span, Expr])
		while: (Maybe[Tuple[Span, Expr]])
		do: (Block)
	]

	has [
		for: (Span)
		var: (Loop.Var)
		start: (Loop.Start)
		stop: (Loop.Stop)
		step: (Maybe[Tuple[Span, Expr]])
		while: (Maybe[Tuple[Span, Expr]])
		do: (Block)
	]

	has [do: (Span), (Block)]

	has [return: (Span)]
	has [return: (Span) value: (Expr)]

	has [break: (Span)]
	has [break: (Span) depth: (Span), (Int)]

	has [next: (Span)]
	has [next: (Span) depth: (Span), (Int)]

	has [throw: (Expr)]

	has [
		try: (Span), (Block)
		catch: (Span)
		cases: (Array[PatternAt])
		else: (Maybe[Tuple[Span, Then]])
	]
}