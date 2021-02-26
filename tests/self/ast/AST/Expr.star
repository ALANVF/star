kind Expr {
	has [int: (Int)]
	has [string: (Str)]
	has [slots: (Array[Slot]) object: (Array[Expr])]
	has [slots: (Array[Slot]) block: (Array[Expr])]
	has [implicitSelf]
	has [message: (Expr) unary: (Str)]
	has [message: (Expr) binary: (Str) arg: (Expr)]
	has [message: (Expr) keywords: (Array[Str]) args: (Array[Expr])]
	has [nonLocalReturn]
	has [nonLocalReturn: (Expr)]
}