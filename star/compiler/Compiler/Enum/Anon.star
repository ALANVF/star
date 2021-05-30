class Anon {
	my cases (Cases)
	
	on [form: (Int) = 0] (Str) {
		return "enum " + cases[:form]
	}
}