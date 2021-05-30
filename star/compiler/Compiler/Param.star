class Param {
	my type (Type)
	my name (Maybe[Str])
	my value = Maybe[Expr][none]
	
	on [form] (Str) {
		return ""
		-> [add: type[form]]
		-> {
			match name at Maybe[the: my name'] {
				this
				-> [add: #" "]
				-> [add: name']
			}
			
			match value at Maybe[the: my value'] {
				this
				-> [add: " = "]
				-> [add: value'[form]]
			}
		}
	}
}

category Param for Array[Param] {
	on [form] (Str) {
		return this[collect: $0[form]][joinWith: ", "]
	}
}