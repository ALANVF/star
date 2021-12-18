kind LoopStart {
	has from
	has after
}

kind LoopStop {
	has to
	has upto
	has downto
	has times
}

kind Stmt {
	has [expr: (Expr)]

	has [
		if: (Expr)
		then: (Stmts)
		else: (Maybe[Stmts])
	]
	has [
		cases: (Array[Tuple[Expr, Stmts]])
		else: (Maybe[Stmts])
	]
	has [
		match: (Expr)
		cases: (Array[Tuple[Pattern, Maybe[Expr], Stmts]])
		else: (Maybe[Stmts])
	]
	has [
		match: (Expr)
		at: (Pattern)
		if: (Maybe[Expr])
		then: (Stmts)
		else: (Maybe[Stmts])
	]

	has [
		while: (Expr)
		label: (Maybe[Str])
		do: (Stmts)
	]
	has [
		label: (Maybe[Str])
		do: (Stmts)
		while: (Expr)
	]

	has [
		for: var (Pattern), var' (Maybe[Pattern])
		in: (Expr)
		while: (Maybe[Expr])
		label: (Maybe[Str])
		do: (Stmts)
	]
	has [
		for: var (Pattern)
		start: (LoopStart)
		stop: (LoopStop)
		step: (Maybe[Expr])
		while: (Maybe[Expr])
		label: (Maybe[Str])
		do: (Stmts)
	]

	has [
		label: (Maybe[Str])
		do: (Stmts)
	]

	has [return]
	has [return: value (Expr)]

	has [break]
	has [breakDepth: (Int)], has [breakLabel: (Str)]

	has [next]
	has [nextDepth: (Int)], has [nextLabel: (Str)]

	has [throw: (Expr) at: span (Span)]
	has [
		try: (Stmts)
		catch: cases (Array[Tuple[Pattern, Maybe[Expr], Stmts]])
		else: (Maybe[Stmts])
	]


	my orig (Maybe[Parser.Stmt])
}

alias Stmts = Array[Stmt]