class Dtor of NamedMethod {
	on [form: (Int) = 0] {
		my buf = ""
		-> [add: attrs[formLeading]]
		
		match path at Maybe[the: my path'] {
			buf
			-> [add: path[form]]
			-> [add: "::"]
		}
		
		buf
		-> [add: "~\(name)()"]
		-> [add: body[:form]]
		
		return buf
	}
}