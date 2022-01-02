class Method of NamedMethod {
	my params (Array[Param])
	my ret (Type)
	my trailingRet = false
	my hasVarargs = false
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf[add: attrs[formLeading]]
		
		if trailingRet {
			buf[add: "auto "]
		} else {
			buf
			-> [add: ret[form]]
			-> [add: #" "]
		}
		
		match path at Maybe[the: my path'] {
			buf
			-> [add: path'[form]]
			-> [add: "::"]
		}
		
		buf
		-> [add: "\(name)("]
		-> [add: params[Param form]]
		
		if hasVarargs {
			buf[add: ", ..."]
		}
		
		buf[add: #")"]
		
		if trailingRet {
			buf
			-> [add: " -> "]
			-> [add: ret[form]]
		}
		
		buf[add: body[:form]]
		
		return buf
	}
}