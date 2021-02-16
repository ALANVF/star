type T
kind Cascade[T] {
	my span (Span)
	my depth (Int)
	my nested (Array[Cascade[Expr]])

	has [member: (Ident)]
	has [member: (Ident) assign: (Span) expr: (Expr)]
	has [member: (Ident) assign: (Span) op: (Infix.Assignable) expr: (Expr)]
	has [member: (Ident) step: (Step)]
	
	has [message: (Message[T])]
	has [message: (Message[T]) assign: (Span) expr: (Expr)]
	has [message: (Message[T]) assign: (Span) op: (Infix.Assignable) expr: (Expr)]
	has [message: (Message[T]) step: (Step)]

	has [block: (Block)]
}