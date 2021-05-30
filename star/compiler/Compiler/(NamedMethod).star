protocol NamedMethod of AnyMethod {
	my template = Maybe[Template][none]
	my name (Str)
}