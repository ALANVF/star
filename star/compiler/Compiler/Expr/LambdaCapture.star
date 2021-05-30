kind LambdaCapture {
	has [expr: (Expr)]
	has [byRef]
	has [byVal]
	
	on [form] (Str) {
		match this {
			at This[expr: my expr] => return expr[form]
			at This[byRef] => return "&"
			at This[byVal] => return "="
		}
	}
}