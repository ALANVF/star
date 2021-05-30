alias Cases (Array[Tuple[Str, Maybe[Expr]]]) {
	on [form: indent (Int) = 0] (Str) {
		my buf = ""
		my indent' = indent + 1
		my ws = "\n" + ("\t" * indent)
		my ws' = ws + "\t"
		
		buf[add: #"{"]
		
		for my i, #{my name, my value} in: this {
			if i != 0 {
				buf[add: #","]
			}
			
			buf
			-> [add: ws']
			-> [add: name]
			
			match value at Maybe[the: my value'] {
				buf
				-> [add: " = "]
				-> [add: value'[form: indent']]
			}
		}
		
		buf
		-> [add: ws]
		-> [add: #"}"]
		
		return buf
	}
}