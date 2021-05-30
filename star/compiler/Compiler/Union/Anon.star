class Anon {
	my body (DeclBody)
	
	on [form: (Int) = 0] (Str) {
		return "union " + body[:form]
	}
}