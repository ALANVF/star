kind Cond of Expr {
	has [varDecl: (VarDecl)]
	
	on [form: (Int) = 0] (Str) {
		match this {
			at This[varDecl: my d] => return d[:form]
			else => return this[Super[Expr] :form]
		}
	}
}

kind Stmt {
	has [none]
	
	has [expr: (Expr)]
	has [varDecl: (VarDecl)]
	has [typeDecl: (TypeDecl)]
	has [block: (Block)]
	
	has [label: (Str) stmt: (Stmt)]
	has [case: (Expr) stmt: (Stmt)]
	has [default: (Stmt)]
	
	has [
		init: (Maybe[VarDecl])
		if: isConstexpr (Bool), cond (Cond)
		then: (Stmt)
		else: (Maybe[Stmt])
	]
	has [init: (Maybe[VarDecl]) switch: (Cond) do: (Stmt)]
	
	has [while: (Cond) do: (Stmt)]
	has [do: (Stmt) while: (Expr)]
	has [
		for: init (Stmt)
		while: cond (Maybe[Expr])
		update: (Maybe[Expr])
		do: (Stmt)
	]
	has [
		init: (Maybe[VarDecl])
		for: (VarDecl)
		in: (Expr)
		do: (Stmt)
	]
	
	has [break]
	has [continue]
	has [return]
	has [return: (Expr)]
	has [goto: label (Str)]
	
	has [
		try: (Block)
		catch: (Array[Tuple[Param, Block]])
		default: (Maybe[Block])
	]
	
	on [form: (Int) = 0] (Str) {
		my indent = form
		my indent' = indent + 1
		
		match this {
			at This[none] => return ";"
			
			at This[expr: my expr] => return expr[:form] + ";"
			
			at This[varDecl: my decl] => return decl[:form] + ";"
			
			at This[typeDecl: my decl] => return decl[:form]
			
			at This[block: my block] => return block[:form]
			
			at This[label: my label stmt: my stmt] {
				return ""
				-> [add: label[Expr fixName]]
				-> [add: ":\n"]
				-> [add: "\t" * indent]
				-> [add: stmt[:form]]
			}
			
			at This[case: my expr stmt: my stmt] {
				return "case \(expr[:form]): \(stmt[form: indent'])"
			}
			
			at This[default: my stmt] => return "default " + stmt[form: indent']
			
			at This[init: my init if: my isConstexpr, my cond then: my then else: my else'] {
				return ""
				-> [add: isConstexpr[yes: "if constexpr(" no: "if("]]
				-> {
					match init at Maybe[the: my init'] {
						this
						-> [add: init'[:form]]
						-> [add: "; "]
					}
				}
				-> [add: cond[:form]]
				-> [add: ") "]
				-> [add: then[form: indent']]
				-> {
					match else' at Maybe[the: my else''] {
						this
						-> [add: " else "]
						-> [add: else''[form: indent']]
					}
				}
			}
			
			at This[init: Maybe[none] switch: my cond do: my stmt] {
				return "switch(\(cond[:form])) \(stmt[:form])"
			}
			at This[init: Maybe[the: my init] switch: my cond do: my stmt] {
				return "switch(\(init[:form]); \(cond[:form])) \(stmt[:form])"
			}
			
			at This[while: my cond do: my stmt] => return "while(\(cond[:form])) \(stmt[:form])"
			
			at This[do: my stmt while: my cond] => return "do \(stmt[:form]) while(\(cond[:form]));"
			
			at This[for: my init while: my cond update: my update do: my stmt] {
				return ""
				-> [add: "for("]
				-> [add: init[:form]]
				-> [maybeAdd: cond[collect: $0[:form]]]
				-> [add: "; "]
				-> [maybeAdd: update[collect: $0[:form]]]
				-> [add: ") "]
				-> [add: stmt[form: indent']]
			}
			
			at This[init: my init for: my var in: my in do: my stmt] {
				return ""
				-> [add: "for("]
				-> {
					match init at Maybe[the: my init'] {
						this
						-> [add: init'[:form]]
						-> [add: "; "]
					}
				}
				-> [add: var[:form]]
				-> [add: ": "]
				-> [add: in[:form]]
				-> [add: ") "]
				-> [add: stmt[form: indent']]
			}
			
			at This[break] => return "break;"
			
			at This[continue] => return "continue;"
			
			at This[return] => return "return;"
			at This[return: my expr] => return "return \(expr[:form]);"
			
			at This[goto: my label] => return "goto \(label);"
			
			at This[try: my try' catch: my catches default: my default] {
				return ""
				-> [add: "try "]
				-> [add: try'[:form]]
				-> {
					for my catch' in: catches {
						this
						-> [add: " catch("]
						-> [add: catch'.first[form]]
						-> [add: ") "]
						-> [add: catch'.second[form: indent']]
					}
					
					match default at Maybe[the: my default'] {
						this
						-> [add: " catch(...) "]
						-> [add: default'[form: indent']]
					}
				}
			}
		}
	}
}