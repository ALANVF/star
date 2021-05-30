class Forward of TypeDecl {
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf
		-> [add: "struct "]
		-> [add: path[form]]	
		-> [add: #";"]
		
		return buf
	}
}