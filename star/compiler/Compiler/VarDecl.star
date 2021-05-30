kind VarInit {
	has [none]
	has [expr: (Expr)]
	has [call: args (Array[Expr])]
	has [initList: (InitList)]
	
	on [form: (Int) = 0] (Str) {
		match this {
			at This[none] => return ""
			at This[expr: my expr] => return " = " + expr[:form]
			at This[call: my args] => return "(" + args[Expr :form] + ")"
			at This[initList: my init] => return init[:form]
		}
	}
}

kind VarDecl {
	my attrs = Attrs.none
	my type (Type)
	my init (VarInit)
	
	has [name: (Str)]
	has [names: (Array[Str])]
	
	on [form: (Int) = 0] (Str) {
		return ""
		-> [add: attrs[formLeading]]
		-> [add: type[form]]
		-> [add: #" "]
		-> [add: {
			match this {
				at This[name: my name] => return name[Expr fixName]
				at This[names: my names] {
					return names[collect: $0[Expr fixName]][joinWith: ", "]
				}
			}
		}]
		-> [add: init[:form]]
	}
}