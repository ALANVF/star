protocol TaggedCase of Decl, HasErrors {
	my decl (AnyTypeDecl)
	my assoc (Maybe[Parser.Message[Parser.Type]]) is getter
	my assoc' (Maybe[Message[Type]]) = Maybe[none]
	my init (Maybe[Array[Parser.Stmt]]) is getter = Maybe[none]
	my init' (Maybe[Stmts]) = Maybe[none]
}