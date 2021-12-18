use #[Prefix, Infix, Suffix] from: Parser

kind StrPart {
	has [str: (Str)]
	has [code: (Expr)]
}

kind Expr {
	has [local: (Local)]

	has [tag: (Str) expr: (Expr)]

	has [int: (Int) exp: (Maybe[Int])]
	has [int: (Int) dec: (Str) exp: (Maybe[Int])]
	has [char: (Char)]
	has [str: parts (Array[StrPart])]
	has [bool: (Bool)]
	has [array: values (Exprs)]
	has [hash: pairs (Array[Tuple[Expr, Expr]])]
	has [tuple: values (Exprs)]
	has [this]
	has [wildcard]
	has [params: (Array[Tuple[Str, Maybe[Type]]]) ret: (Maybe[Type]) func: body (Stmts)]
	has [depth: (Int) anonArg: nth (Int)]
	has [type: (Type) ctor: (Expr)]

	has [paren: exprs (Exprs)]
	has [block: stmts (Stmts)]

	has [type: (Type) msg: (Message[Type])]
	has [type: (Type) cascades: (Array[Cascade[Type]])]
	has [type: (Type) member: kind (SingleStatic)]

	has [expr: (Expr) msg: (Message[Expr])]
	has [expr: (Expr) cascades: (Array[Cascade[Expr]])]
	has [expr: (Expr) lazyMember: name (Str)]
	has [expr: (Expr) member: kind (SingleInst)]

	has [prefix: kind (UnaryOp) right: (Expr)]
	has [lazyPrefix: (Prefix) right: (Expr)]
	
	has [left: (Expr) suffix: kind (UnaryOp)]
	has [left: (Expr) lazySuffix: (Suffix)]
	
	has [left: (Expr) infix: kinds (BinaryOps) right: (Expr)]
	has [left: (Expr) lazyInfix: (Infix) right: (Expr)]
	has [left: (Expr) infixChain: (Array[Tuple[BinaryOps, Expr]])]

	has [my: name (Str) type: (Maybe[Type]) value: (Maybe[Expr])]

	;== Assignment
	has [local: (Local) set: value (Expr)]

	;== From tags
	has [initThis: (Type) msg: (Message[Type])]
	has [inline: (Expr)]
	has [kindId: (Expr)]
	has [kindSlot: (Expr) at: index (Int)]

	;== Macros
	;has [macroQuote: (Expr)]
	;has [macroExpand: (Expr)]

	;== Misc
	has [invalid]


	my t (Maybe[Type]) = Maybe[none]
	my orig (Maybe[Parser.Expr]) = Maybe[none]
}

alias Exprs = Array[Expr]