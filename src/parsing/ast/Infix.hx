package parsing.ast;

private class _Assignable {}

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
	
	Assign(op: Option<_Infix<_Assignable>>);
}

typedef Infix = _Infix<Dynamic>;

typedef Assignable = _Infix<_Assignable>;
