kind MemberInit {
	my init (InitCtor)
	
	has [var: name (Str)]
	has [type: (Type)]
	
	on [form] (Str) {
		match this {
			at This[var: my name] => return name + init[form]
			at This[type: my type] => return type[form] + init[form]
		}
	}
}

category MemberInit for Array[MemberInit] {
	on [form] (Str) {
		return this[collect: MemberInit$0[form]][joinWith: ", "]
	}
}