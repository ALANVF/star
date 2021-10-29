kind Expr {
	has [nullptr]
	has [this]
	has [bool: (Bool)]
	has [int: (Int) exp: (Maybe[Int])]
	has [float: int (Int), dec (Str) exp: (Maybe[Int])]
	has [char: (Char)]
	has [string: (Str)]
	has [name: (Str)]
	has [paren: (This)]
	has [initList: (InitList)]
	has [type: (Type) ctor: (InitCtor)]
	
	has [
		captures: (Array[LambdaCapture])
		template: (Maybe[Template])
		params: (Array[Param])
		attrs: (Attrs)
		return: (Maybe[Type])
		requires: (Maybe[Requires])
		lambda: (Block)
	]
	
	has [
		placement: (Maybe[Array[This]])
		constraint: (Maybe[Type])
		new: (Type)
		hasTypeParens: (Bool)
		ctor: (Maybe[InitCtor])
	]
	
	has [delete: (Expr)]
	has [deleteArray: (Expr)]
	
	has [throw]
	has [throw: (Expr)]
	
	has [cast: (Type) expr: (Expr)]
	has [constCast: (Type) expr: (Expr)]
	has [staticCast: (Type) expr: (Expr)]
	has [dynamicCast: (Type) expr: (Expr)]
	has [reinterpretCast: (Type) expr: (Expr)]
	
	has [op: (Prefix) right: (This)]
	has [left: (This) op: (Suffix)]
	has [left: (This) op: (Infix) right: (This)]
	has [expr: (Expr) index: (Expr)]
	has [call: (Expr) typeArgs: (Maybe[Array[Type]]) args: (Array[This])]
	
	has [expr: (Expr) dot: name (Str)]
	has [expr: (Expr) path: (TypePath) dot: name (Str)]
	has [expr: (Expr) dotTemplate: name (Str)]
	has [expr: (Expr) dotRef: (Expr)]
	
	has [expr: (Expr) arrow: name (Str)]
	has [expr: (Expr) path: (TypePath) arrow: name (Str)]
	has [expr: (Expr) arrowTemplate: name (Str)]
	has [expr: (Expr) arrowRef: (Expr)]
	
	has [type: (Type) scope: name (Str)]
	
	has [if: cond (Expr) then: (Expr) else: (Expr)]
	
	has [sizeof: (Expr)]
	has [sizeofPack: (Expr)]
	has [alignof: (Expr)]
	has [typeid: (Expr)]
	
	has [raw: code (Str)]
	
	on [form: ;[indent] (Int) = 0] (Str) {
		match this {
			at This[nullptr] => return "nullptr"
			
			at This[this] => return "this"
			
			at This[bool: true] => return "true"
			at This[bool: false] => return "false"
			
			at This[int: my int exp: Maybe[none]] => return int[Str]
			at This[int: my int exp: Maybe[the: my exp]] => return "\(int)e\(exp)"
			
			at This[float: my int, my dec exp: Maybe[none]] => return "\(int).\(dec)"
			at This[float: my int, my dec exp: Maybe[the: my exp]] => return "\(int).\(dec)e\(exp)"
			
			at This[char: my char] => return "'\(char[Expr escape])'"
			
			at This[string: my str] => return "\"\(str[Expr escape])\""
			
			at This[name: my name] => return name[Expr fixName]
			
			at This[paren: my expr] => return "(\(expr[:form]))"
			
			at This[initList: my init] => return init[:form]
			
			at This[type: my type ctor: my ctor] => return type[form] + ctor[form]
			
			at This[
				captures: my captures
				template: my template
				params: my params
				attrs: my attrs
				return: my ret
				requires: my requires
				lambda: my block
			] {
				return ""
				-> [add: #"["]
				-> [add: captures[collect: $0[form]][joinWith: ", "]]
				-> [add: #"]"]
				-> {
					match template at Maybe[the: my tmpl] {
						this[add: tmpl[:form]->[removeFirst: "template"]]
					}
				}
				-> [add: #"("]
				-> [add: params[collect: $0[form]][joinWith: ", "]]
				-> [add: #")"]
				-> [add: attrs[formTrailing]]
				-> {
					match ret at Maybe[the: my ret'] {
						this
						-> [add: " -> "]
						-> [add: ret'[:form]]
					}
					
					match requires at Maybe[the: my req] {
						this
						-> [add: " requires "]
						-> [add: req[:form]]
					}
				}
				-> [add: #" "]
				-> [add: block[:form]]
			}
			
			at This[placement: Maybe[none] constraint: Maybe[none] new: my type hasTypeParens: my parens ctor: my ctor] {
				return parens[yes: "new (\(type[form]))" no: "new \(type[form])"] + {
					match ctor at Maybe[the: my ctor'] {
						return ctor'[form]
					} else {
						return ""
					}
				}
			}
			at This[placement: _ constraint: _ new: _ hasTypeParens: _ ctor: _] => throw "todo!" ; I'll probably never need this
			
			at This[delete: my expr] => return "delete " + expr[form]
			at This[deleteArray: my expr] => return "delete[] " + expr[form]
			
			at This[throw] => return "throw"
			at This[throw: my expr] => return "throw " + expr[form]
			
			at This[cast: my type expr: my expr] => return "((\(type[form]))(\(expr[form])))"
			
			at This[constCast: my type expr: my expr] => return "const_cast<\(type[form])>(\(expr[form]))"
			
			at This[staticCast: my type expr: my expr] => return "static_cast<\(type[form])>(\(expr[form]))"
			
			at This[dynamicCast: my type expr: my expr] => return "dynamic_cast<\(type[form])>(\(expr[form]))"
			
			at This[reinterpretCast: my type expr: my expr] => return "reinterpret_cast<\(type[form])>(\(expr[form]))"
			
			at This[op: my op right: my right] => return op[form] + right[form]
			
			at This[left: my left op: my op] => return left[form] + op[form]
			
			at This[left: my left op: my op right: my right] => return left[form] + op[form] + right[form]
			
			at This[expr: my expr index: my index] => return "\(expr[form])[\(index[form])]"
			
			at This[call: my expr typeArgs: Maybe[none] args: my args] => return "\(expr[form])(\(args[Expr form]))"
			at This[call: my expr typeArgs: Maybe[the: my types] args: my args] => return "\(expr[form])<\(types[Type form])>(\(args[Expr form]))"
			
			at This[expr: my expr dot: my name] => return expr[form] + "." + name[Expr fixName]
			at This[expr: my expr path: my path dot: my name] => return "\(expr[form]).\(path[form])::\(name[Expr fixName])"
			at This[expr: my expr dotTemplate: my name] => return expr[form] + ".template" + name[Expr fixName]
			at This[expr: my expr dotRef: my ref] => return expr[form] + ".*" + ref[form]
			
			at This[expr: my expr arrow: my name] => return expr[form] + "->" + name[Expr fixName]
			at This[expr: my expr path: my path arrow: my name] => return "\(expr[form])->\(path[form])::\(name[Expr fixName])"
			at This[expr: my expr arrowTemplate: my name] => return expr[form] + "->template" + name[Expr fixName]
			at This[expr: my expr arrowRef: my ref] => return expr[form] + "->*" + ref[form]
			
			at This[type: my type scope: my name] => return type[form] + "::" + name[Expr fixName]
			
			at This[if: my cond then: my yes else: my no] => return "\(cond[form]) ? \(yes[form]) : \(no[form])"
			
			at This[sizeof: my expr] => return "sizeof(" + expr[form] + ")"
			at This[sizeofPack: my expr] => return "sizeof...(" + expr[form] + ")"
			
			at This[alignof: my expr] => return "alignof(" + expr[form] + ")"
			
			at This[typeid: my expr] => return "typeid(" + expr[form] + ")"
			
			at This[raw: my code] => return code[new]
		}
	}
}

category Expr for Array[Expr] {
	on [form: (Int) = 0] (Str) {
		return this[collect: $0[:form]][joinWith: ", "]
	}
}