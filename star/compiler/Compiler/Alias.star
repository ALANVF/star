class Alias of TypeDecl {
	my type (Type)
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf
		-> [add: "using "]
		-> [add: path[form]]
		-> [add: " = "]
		-> [add: type[form]]
		-> [add: #";"]
		
		return buf
	}
}