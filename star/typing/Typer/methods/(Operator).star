protocol Operator of RealMethod {
	my ret (Maybe[Type])
	my opSpan (Span) is getter
	my isInline (Bool) = false
	my isMacro (Bool) = false
}