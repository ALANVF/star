kind Const of Expr {
	has [requires: (Requires)]
	has [type: (Type)]
	
	on [form: (Int) = 0] (Str) {
		match this {
			at This[requires: my req] => return req[:form]
			at This[type: my type] => return type[form]
			else => return this[Super[Expr] :form]
		}
	}
}