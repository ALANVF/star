class Ctor of NamedMethod {
	my params (Array[Param])
	my hasVarargs = false
	my inits (Array[MemberInit])
	
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
			-> [add: path[form]]
			-> [add: "::"]
		}
		
		buf
		-> [add: "\(name)("]
		-> [add: params[Param form]]
		
		if hasVarargs {
			buf[add: ", ..."]
		}
		
		buf[add: #")"]
		
		if inits.length > 0 {
			buf
			-> [add: ": "]
			-> [add: inits[MemberInit form]]
		}
		
		buf[add: body[:form]]
		
		return buf
	}
}