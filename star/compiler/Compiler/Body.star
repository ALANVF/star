kind Body {
	has [none]
	has [block: (Block)]
	has [pure]
	has [default]
	has [disable]
	
	on [form: (Int) = 0] (Str) {
		match this {
			at This[none] => ";"
			at This[block: my block] => return " \(block[:form])"
			at This[pure] => return " = 0;"
			at This[default] => return " = default;"
			at This[disable] => return " = delete;"
		}
	}
}