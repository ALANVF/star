use Ident from: Parser

class Member of Decl, TypeLookup {
	my lookup (TypeLookup)
	my ident (Ident)
	my type (Maybe[Type])
	; ...
	my value (Maybe[Parser.Expr])

	on [name] (Str) is getter => return ident.name
}