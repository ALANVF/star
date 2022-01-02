class MemberList {
	class Part {
		my name (Str)
		my bits = Maybe[Expr][none]
		my init (MemberInit)
		
		on [form: (Int)] (Str) {
			my buf = ""
			-> [add: name]
			
			match bits at Maybe[the: my bits'] {
				buf
				-> [add: ": "]
				-> [add: bits'[:form]]
			}
			
			buf[add: init[form]]
			
			return buf
		}
	}
	
	my attrs = Attrs.none
	my type (Type)
	my parts (Array[Part])
	
	on [form: indent (Int) = 0] (Str) {
		my buf = ""
		my indent' = indent + 1
		my ws' = "\n" + ("\t" * indent')
		
		buf
		-> [add: attrs[formLeading]]
		-> [add: type[form]]
		
		for my part in: parts {
			buf
			-> [add: ws']
			-> [add: part[form: indent']]
		}
		
		return buf
	}
}