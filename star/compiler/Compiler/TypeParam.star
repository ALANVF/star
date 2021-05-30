class TypeParam {
	my template = Maybe[Template][none]
	my type = Maybe[Type][none]
	my isVariadic = false
	my name = Maybe[Str][none]
	my default = Maybe[Type][none]
	
	on [form: (Int) = 0] (Str) {
		my buf = ""
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[:form]]
			-> [add: #" "]
		}
		
		buf[add: {
			match type at Maybe[the: my type'] {
				return type'[form]
			} else {
				return "typename"
			}
		}]
			
		if isVariadic {
			buf[add: "..."]
		}
		
		match name at Maybe[the: my name'] {
			buf
			-> [add: #" "]
			-> [add: name']
		}
		
		match default at Maybe[the: my default'] {
			buf
			-> [add: " = "]
			-> [add: default'[:form]]
		}
		
		return buf
	}
}