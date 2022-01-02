use Ident from: Parser

class ValueCase of Decl, HasErrors {
	my decl (AnyTypeDecl) is getter
	my name (Ident) is getter
	my value (Maybe[Parser.Expr])
	my value' (Maybe[Expr]) = Maybe[none]
}