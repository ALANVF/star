kind Access {
	has static
	has instance

	on [desc] (Str) is getter {
		match this {
			at This.static => return "Type"
			at This.instance => return "Value of type"
		}
	}
}