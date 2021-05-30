alias Block (Array[Stmt]) {
	on [form: indent (Int) = 0] (Str) {
		if this.length ?= 0 {
			return "{}"
		} else {
			my indent' = indent + 1
			my ws = "\n" + ("\t" * indent)
			my ws' = ws + "\t"
			
			my buf = "{"
			
			for my stmt in: this {
				buf
				-> [add: ws']
				-> [add: stmt[form: indent']]
			}
			
			return buf
			-> [add: ws]
			-> [add: #"}"]
		}
	}
}