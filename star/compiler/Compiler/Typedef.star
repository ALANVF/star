class Typedef of TypeDecl {
	my type (Type)
	
	on [form: (Int) = 0] (Str) {
		return ""
		-> [add: "typedef "]
		-> [add: type[form]]
		-> [add: #" "]
		-> [add: path[form]]
		-> [add: #";"]
	}
}