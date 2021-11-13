type T
kind Cascade[T] {
	my span (Span)
	my level (Int)
	my nested (Cascades[Expr]) = #[]

	has [member: (Ident)]
	has [member: (Ident) assign: (Span) expr: (Expr)]
	has [member: (Ident) assign: (Span) op: (Infix.Assignable) expr: (Expr)]
	has [member: (Ident) step: (Span), (Step)]
	
	has [message: (Message[T])]
	has [message: (Message[T]) assign: (Span) expr: (Expr)]
	has [message: (Message[T]) assign: (Span) op: (Infix.Assignable) expr: (Expr)]
	has [message: (Message[T]) step: (Span), (Step)]

	has [block: (Block)]
}

type T
alias Cascades[T] = Array[Cascade[T]]