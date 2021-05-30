protocol AnyMethod {
	my attrs = Attrs.none
	my path = Maybe[TypePath][none]
	my body = Body[none]
	
	on [form: (Int) = 0] (Str)
}