kind InitList {
	has [exprs: (Array[Expr])]
	has [named: (Array[Tuple[String, Expr]])]
	has [namedInit: (Array[Tuple[String, InitList]])]
	
	on [form: (Int) = 0] (Str) {
		return "{"
		-> [add: {
			match this {
				at This[exprs: my exprs] => return exprs[Expr form]
				at This[named: my names] {
					return names[collect: ".\($.0.first[Expr fixName]) = \($.0.second[:form])"][joinWith: ", "]
				}
				at This[namedInit: my names] {
					return names[collect: "." + $0.first[Expr fixName] + $0.second[:form]][joinWith: ", "]
				}
			}
		}]
		-> [add: #"}"]
	}
}