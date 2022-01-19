protocol TypeDecl {
	my template (Maybe[Template]) = Maybe[none]
	my path (TypePath)
	
	on [form: indent (Int) = 0] (Str)
}