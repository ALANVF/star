class Template {
	my types (Array[TypeParam])
	my requires = Maybe[Expr.Const][none]
	
	on [form: indent (Int) = 0] (Str) {
		return ""
		-> [add: "template<"]
		-> [add: types[collect: $0[form: indent]][joinWith: ", "]]
		-> {
			match requires at Maybe[the: my req] {
				this
				-> [add: "> requires "]
				-> [add: req[form: indent]]
			} else {
				this[add: #">"]
			}
		}
	}
}