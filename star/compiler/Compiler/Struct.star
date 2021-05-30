class Struct of ClassLike {
	on [form: (Int) = 0] (Str) {
		return this[keyword: "struct " :form]
	}
}