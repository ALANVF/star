class Operator of AnyMethod {
	my template = Maybe[Template][none]
	my op (Str)
	my params (Array[Param])
	my ret (Type)
	my trailingRet = false
	
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
			-> [add: path[form]]
			-> [add: "::"]
		}
		
		buf
		-> [add: "operator \(op)("]
		-> [add: params[Param form]]
		-> [add: #")"]
		
		if trailingRet {
			buf
			-> [add: " -> "]
			-> [add: ret[form]]
		}
		
		buf[add: body[:form]]
		
		return buf
	}
}