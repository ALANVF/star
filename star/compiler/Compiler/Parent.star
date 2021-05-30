class Parent {
	my attrs = Array[Str] #[]
	my type (Type)
	
	on [form] (Str) {
		return attrs[collect: $0+#" "][join] + type[form]
	}
}

category Parent for Array[Parent] {
	on [form] (Str) {
		return this[collect: $0[form]][joinWith: ", "]
	}
}