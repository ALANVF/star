use Ident from: Parser

class Member of Decl, TypeLookup {
	my decl (AnyTypeDecl)
	my ident (Ident)
	my type (Maybe[Type])
	my isStatic (Bool) = false
	my hidden (Maybe[Maybe[Type]]) = Maybe[none]
	my isReadonly (Bool) = false
	my getter (Maybe[Maybe[Ident]]) = Maybe[none]
	my setter (Maybe[Maybe[Ident]]) = Maybe[none]
	my noInherit (Bool) = false
	my value (Maybe[Parser.Expr])
	my value' (Maybe[Expr]) = Maybe[none]

	on [name] (Str) is getter => return ident.name
}