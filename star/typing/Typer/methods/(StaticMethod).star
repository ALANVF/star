protocol StaticMethod of RealMethod {
	my ret (Maybe[Type])
	my isMain (Bool) = false
	my isGetter (Bool) = false
	my isSetter (Bool) = false
	my isInline (Bool) = false
	my isMacro (Bool) = false
}