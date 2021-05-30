kind Using {
	has [type: (Type)]
	has [kind: (Str) type: (Type)]
	has [expr: (Expr)]
	
	on [form] (Str) {
		match this {
			at This[type: my type] => return "using \(type[form]);"
			at This[kind: my kind type: my type] => return "using \(kind) \(type[form]);"
			at This[expr: my expr] => return "using \(expr[form]);s"
		}
	}
}