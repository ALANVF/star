;alias MemberInit (VarInit)

class Member {
	my template = Maybe[Template][none]
	my attrs = Attrs.none
	my type (Type)
	my path = Maybe[TypePath][none]
	my name (Str)
	my bits = Maybe[Expr][none]
	my init = VarInit[none]
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf
		-> [add: attrs[formLeading]]
		-> [add: type[form]]
		-> [add: #" "]
		
		match path at Maybe[the: my path'] {
			buf
			-> [add: path'[form]]
			-> [add: "::"]
		}
		
		buf[add: name]
		
		match bits at Maybe[the: my bits'] {
			buf
			-> [add: ": "]
			-> [add: bits'[:form]]
		}
		
		buf
		-> [add: init[:form]]
		-> [add: #";"]
		
		return buf
	}
}