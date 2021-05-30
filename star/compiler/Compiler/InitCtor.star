kind InitCtor {
	has [call: args (Array[Expr])]
	has [initList: (InitList)]
	
	on [form] (Str) {
		match this {
			at This[call: my args] => return "(" + args[Expr form] + ")"
			at This[initList: my init] => return init[form]
		}
	}
}