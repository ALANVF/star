kind Type {
	has [void]
	has [bool]
	has [int]
	has [int8]
	has [uint8]
	has [int16]
	has [uint16]
	has [int32]
	has [uint32]
	has [int64]
	has [uint64]
	has [size_t]
	has [float]
	has [double]
	has [longDouble]
	
	has [char]
	has [char8_t]
	
	has [unsigned: (Type)]
	has [signed: (Type)]
	
	has [const: (Type)]
	has [volatile: (Type)]
	
	has [ptr: (Type)]
	has [lval: (Type)]
	has [rval: (Type)]
	has [array]
	has [array: (Type)]
	has [array: (Type) size: (Expr)]
	
	has [return: (Type) params: (Array[Type])]
	
	has [struct: (DeclBody)]
	has [structPath: (TypePath)]
	has [path: (TypePath) struct: (DeclBody)]
	
	has [union: (Union.Anon)]
	has [unionPath: (TypePath)]
	has [path: (TypePath) union: (DeclBody)]
	
	has [enum: (Enum.Anon)]
	has [enumPath: (TypePath)]
	has [path: (TypePath) enum: (Enum.Cases)]
	
	has [typename: (Type)]
	has [path: (TypePath)]
	has [type: (Type) path: (TypePath)]
	
	has [auto]
	has [auto: constraint (Type)]
	has [decltype: (Expr)]
	has [decltypeAuto]
	has [decltypeAuto: constraint (Type)]
	
	has [pack: (Type)]
	
	has [expr: (Expr;[.Const])]

	
	on [fromType: type (Typer.Type) in: cmp (Compiler)] (This) is static {
		match type {
			at Typer.Type[depth: _ lookup: my path source: _] => throw "todo"
			at Typer.Type[type: my base lookup: my lookup] => throw "todo"
			at Typer.Type[decl: my decl] => return This[path: cmp[getFullPath: decl]]
			at Typer.Type[decl: my decl params: my params ctx: my ctx] => throw "todo"
			at Typer.Type[this: _] => This[path: TypePath[named: "$This"]]
			at Typer.Type[blank] => throw "todo"
			at Typer.Type[types: my types] => throw "todo"
			at Typer.Type[type: Typer.Type[this: _] args: my args] {
				return This[path: TypePath[named: "$This" of: args[collect: Type[fromType: Typer.Type$.0 in: cmp]]]]
			}
			at Typer.Type[type: my type' args: my args] => throw "todo"
			at Typer.Type[typevar: my typevar] => throw "todo"
			at Typer.Type[type: my type' unit: _] => return This[fromType: type' in: cmp]
		}
	}

	
	on [form] (Str) {
		match this {
			at This[void] => return "void"
			at This[bool] => return "bool"
			at This[int] => return "int"
			at This[int8] => return "int8_t"
			at This[uint8] => return "uint8_t"
			at This[int16] => return "int16_t"
			at This[uint16] => return "uint16_t"
			at This[int32] => return "int32_t"
			at This[uint32] => return "uint32_t"
			at This[int64] => return "int64_t"
			at This[uint64] => return "uint64_t"
			at This[size_t] => return "size_t"
			at This[float] => return "float"
			at This[double] => return "double"
			at This[longDouble] => return "long double"
			
			at This[char] => return "char"
			at This[char8_t] => return "char8_t"
			
			at This[unsigned: my t] => return "unsigned" + t[form]
			at This[signed: my t] => return "signed " + t[form]
			
			at This[const: my t] => return t[form] + " const"
			at This[volatile: my t] => return t[form] + " volatile"
			
			at This[ptr: my t] => return t[form] + "*"
			at This[lval: my t] => return t[form] + "&"
			at This[rval: my t] => return t[form] + "&&"
			at This[array: my t] => return t[form] + "[]"
			at This[array: my t size: my s] => return t[form]+"[\(s[form])]"
			
			at This[return: my r params: my p] => return r[form]+"(\(p[Type form]))"
			
			at This[struct: my s] => return "struct " + s[form]
			at This[structPath: my p] => return "struct " + p[form]
			at This[path: my p struct: my s] => return "struct \(p[form]) \(s[form])"
			
			at This[union: my u] => return u[form]
			at This[unionPath: my p] => return "union " + p[form]
			at This[path: my p union: my u] => return "union \(p[form]) \(u[form])"
			
			at This[enum: my e] => return e[form]
			at This[enumPath: my p] => return "enum " + p[form]
			at This[path: my p enum: my e] => return "enum \(p[form]) \(e[form])"
			
			at This[typename: my t] => return "typename " + t[form]
			at This[path: my p] => return p[form]
			at This[type: my t path: my p] => return t[form] + "::" + p[form]
			
			at This[auto] => return "auto"
			at This[auto: my c] => return c[form] + " auto"
			at This[decltype: my e] => return "decltype(\(e[form]))"
			at This[decltypeAuto] => return "decltype(auto)"
			at This[decltypeAuto: my c] => return c[form] + " decltype(auto)"
			
			at This[pack: my t] => return t[form] + "..."
			
			at This[expr: my e] => return e[form]
		}
	}
}

category Type for Array[Type] {
	on [form] (Str) {
		return this[collect: Type$0[form]][joinWith: ", "]
	}
}