use Ident from: Parser

protocol RealMethod of AnyMethod {
	my hidden (Maybe[Maybe[Type]]) = Maybe[none]
	my noInherit (Bool) = false
	my native (Maybe[Maybe[Ident]]) = Maybe[none]
	my isAsm (Bool) = false
	my body (Maybe[Array[Parser.Stmt]]) is getter
	my body' (Maybe[Stmts]) = Maybe[none]
}