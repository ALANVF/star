class Namespace {
	my path (Maybe[TypePath])
	my body (DeclBody)
	
	on [form: (Int) = 0] (Str) {
		match path at Maybe[the: my path'] {
			return "namespace \(path'[form]) \(body[:form])"
		} else {
			return "namespace \(body[:form])"
		}
	}
}