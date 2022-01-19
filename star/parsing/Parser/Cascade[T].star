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


	on [span: (Span) level: (Int) member: (Ident) assign: (Span) op: (Maybe[Infix.Assignable]) expr: (Expr)] (This) is static {
		match op at Maybe[the: my op'] {
			return This[:span :level :member :assign op: op' :expr]
		} else {
			return This[:span :level :member :assign :expr]
		}
	}

	on [span: (Span) level: (Int) message: (Message[T]) assign: (Span) op: (Maybe[Infix.Assignable]) expr: (Expr)] (This) is static {
		match op at Maybe[the: my op'] {
			return This[:span :level :message :assign op: op' :expr]
		} else {
			return This[:span :level :message :assign :expr]
		}
	}
}

type T
alias Cascades[T] = Array[Cascade[T]]