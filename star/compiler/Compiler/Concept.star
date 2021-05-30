class Concept of TypeDecl {
	my cond (Expr.Const)
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf
		-> [add: "concept "]
		-> [add: path[form]]
		-> [add: " = "]
		-> [add: cond[:form]]
		-> [add: ";"]
		
		return buf
	}
}