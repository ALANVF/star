kind Expr {
	has [name: (Ident)]
	has [litsym: (Ident)]

	has [tag: (Ident) expr: (This)]

	has [span: (Span) int: (Int)]
	has [span: (Span) int: (Int) exp: (Int)]
	has [span: (Span) int: (Int) dec: (Str)]
	has [span: (Span) int: (Int) dec: (Str) exp: (Int)]
	has [span: (Span) char: (Char)]
	has [span: (Span) str: (Array[StrPart])]
	has [span: (Span) bool: (Bool)]
	has [begin: (Span) array: (Array[This]) end: (Span)]
	has [begin: (Span) hash: (Array[Tuple[This, This]]) end: (Span)]
	has [begin: (Span) tuple: (Array[This]) end: (Span)]
	has [this: (Span)]
	has [wildcard: (Span)]
	has [
		begin: (Span)
		params: (Array[Tuple[Ident, Maybe[Type]]])
		return: (Maybe[Type])
		func: (Array[Stmt])
		end: (Span)
	]
	has [span: (Span) anonArg: (Int) depth: (Int)]
	has [type: (Type) literal: (Expr)] ;-- maybe make a Literal supertype instead?

	has [begin: (Span) paren: (Array[This]) end: (Span)]
	has [block: (Block)]

	has [type: (Type) begin: (Span) message: (Message[Type]) end: (Span)]
	has [type: (Type) cascades: (Array[Cascade[Type]])]
	has [type: (Type) member: (Ident)]
	
	has [expr: (This) begin: (Span) message: (Message[This]) end: (Span)]
	has [expr: (This) cascades: (Array[Cascade[This]])]
	has [expr: (This) member: (Ident)]

	has [prefix: (Span), (Prefix) right: (This)]
	has [left: (This) suffix: (Span), (Suffix)]
	has [left: (This) infix: (Span), (Infix) right: (This)]

	; maybe remove
	has [type: (Type)]
}