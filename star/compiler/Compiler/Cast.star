class Cast of AnyMethod {
	my template = Maybe[Template][none]
	my type (Type)
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf[add: attrs[formLeading]]
		
		match path at Maybe[the: my path'] {
			buf
			-> [add: path'[form]]
			-> [add: "::"]
		}
		
		buf
		-> [add: "operator "]
		-> [add: type[form]]
		-> [add: "()"]
		-> [add: body[:form]]
		
		return buf
	}
}