package parsing.ast;

private class _Assignable {}

@:using(parsing.ast.Infix)
private enum _Infix<T> {
	Plus: _Infix<_Assignable>;
	Minus: _Infix<_Assignable>;
	Times: _Infix<_Assignable>;
	Pow: _Infix<_Assignable>;
	Div: _Infix<_Assignable>;
	IntDiv: _Infix<_Assignable>;
	Mod: _Infix<_Assignable>;
	IsMod: _Infix<_Assignable>;
	BitAnd: _Infix<_Assignable>;
	BitOr: _Infix<_Assignable>;
	BitXor: _Infix<_Assignable>;
	Shl: _Infix<_Assignable>;
	Shr: _Infix<_Assignable>;
	And: _Infix<_Assignable>;
	Or: _Infix<_Assignable>;
	Xor: _Infix<_Assignable>;
	Nor: _Infix<_Assignable>;

	Eq;
	Ne;
	Gt;
	Ge;
	Lt;
	Le;
	
	Assign(?op: _Infix<_Assignable>);
}

@:using(parsing.ast.Infix)
typedef Infix = _Infix<Dynamic>;

@:using(parsing.ast.Infix)
typedef Assignable = _Infix<_Assignable>;


function symbol<T>(self: _Infix<T>) return self._match(
	at(Plus) => "+",
	at(Minus) => "-",
	at(Times) => "*",
	at(Pow) => "**",
	at(Div) => "/",
	at(IntDiv) => "//",
	at(Mod) => "%",
	at(IsMod) => "%%",
	at(BitAnd) => "&",
	at(BitOr) => "|",
	at(BitXor) => "^",
	at(Shl) => "<<",
	at(Shr) => ">>",
	at(And) => "&&",
	at(Or) => "||",
	at(Xor) => "^^",
	at(Nor) => "!!",
	at(Eq) => "?=",
	at(Ne) => "!=",
	at(Gt) => ">",
	at(Ge) => ">=",
	at(Lt) => "<",
	at(Le) => "<=",
	at(Assign(null)) => "=",
	at(Assign(op!!)) => op.symbol()+"="
);