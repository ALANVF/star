protocol TypeDecl {
	my template = Maybe[Template][none]
	my path (TypePath)
	
	on [form: indent (Int) = 0] (Str)
}