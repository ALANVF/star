type T
kind Message[T] {
	my category (Maybe[Type])

	has [single: name (Ident)]
	has [multi: labels (Array[Label])]
}

kind Message[Expr] {
	has [cast: type (Type)]
}