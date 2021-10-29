class ClassBody {
	my default = DeclBody #[]
	my private = DeclBody #[]
	my protected = DeclBody #[]
	my public = DeclBody #[]
}

protocol ClassLike of TypeDecl {
	my parents = Array[Parent] #[]
	my body = ClassBody[new]
	
	on [keyword: (Str) form: indent (Int)] (Str) is hidden {
		my buf = ""
		my indent' = indent + 1
		my ws = "\n" + ("\t" * indent)
		my ws' = ws + "\t"
		
		match template at Maybe[the: my template'] {
			buf
			-> [add: template'[form: indent]]
			-> [add: #" "]
		}
		
		buf
		-> [add: keyword]
		-> [add: path[form]]
		
		if parents.length > 0 {
			buf
			-> [add: ": "]
			-> [add: parents[Parent form]]
		}
		
		buf[add: " {"]
		
		for my decl in: body.normal {
			buf
			-> [add: ws']
			-> [add: decl[form: indent']]
		}
		
		for my label, my section in: #(
			"private:" => body.private
			"protected:" => body.protected
			"public:" => body.public
		) {
			if section.length > 0 {
				buf
				-> [add: ws]
				-> [add: label]
				
				for my decl in: section {
					buf
					-> [add: ws']
					-> [add: decl[form: indent']]
				}
			}
		}
		
		buf
		-> [add: ws]
		-> [add: "};"]
		
		return buf
	}
}