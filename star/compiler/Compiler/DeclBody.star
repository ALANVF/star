alias DeclBody (Array[DeclStmt]) {
	on [form: indent (Int) = 0] (Str) {
		my buf = ""
		my indent' = indent + 1
		my ws = "\n" + ("\t" * indent)
		my ws' = ws + "\t"
		
		buf[add: #"{"]
		
		for my stmt in: this {
			buf
			-> [add: ws']
			-> [add: stmt[form: indent']]
		}
		
		buf
		-> [add: ws]
		-> [add: #"}"]
		
		return buf
	}
}