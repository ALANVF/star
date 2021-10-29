kind Req of Stmt {
	has [compound: (Expr)]
	has [compound: (Expr) requires: (Type)]
	has [typename: (Type)]
	has [requires: (Expr.Const)]
	
	on [form: (Int)] (Str) {
		match this {
			at This[compound: my expr] => return "{ \(expr[:form]) };"
			at This[compound: my expr requires: my type] => return "{ \(expr[:form]) } -> \(type[form]);"
			at This[typename: my type] => return "typename \(type[form]);"
			at This[requires: my req] => return req[:form]
			else => return this[Super[Stmt] :form]
		}
	}
}

class Requires {
	my params (Array[Param])
	my body (Array[Req])
	
	on [form: (Int) = 0] (Str) {
		my res = "requires"
		my indent = form
		my indent' = indent + 1
		my ws = "\n" + ("\t" * indent)
		my ws' = ws + "\t"
		
		if params? {
			res
			-> [add: #"("]
			-> [add: params[collect: $0[form]][joinWith: ", "]]
			-> [add: #")"]
		}
		
		res[add: " {"]
		
		for my req in: body {
			res
			-> [add: ws']
			-> [add: req[form: indent']]
		}
		
		res
		-> [add: ws]
		-> [add: #"}"]
		
		return res
	}
}