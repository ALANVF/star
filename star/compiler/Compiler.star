class Compiler {
	my errors = Array[Diagnostic] #[]
	my stmts = #[
		DeclStmt[includeLib: "cstdint"]
		DeclStmt[includeLib: "type_traits"]
		DeclStmt[typeDecl: Alias[
			template: Maybe[the: Template[
				types: #[
					TypeParam[
						name: Maybe[the: "Ret"]
					]
					TypeParam[
						name: Maybe[the: "Args"]
						isVariadic: true
					]
				]
			]]
			path: TypePath[named: "$Func"]
			type: Type[
				return: Type[path: TypePath[named: "Ret"]]
				params: #[
					Type[pack: Type[path: TypePath[named: "Args"]]]
				]
			]
		]]
		DeclStmt[typeDecl: Alias[
			template: Maybe[the: Template[
				types: #[
					TypeParam[
						name: Maybe[the: "Elem"]
					]
				]
			]]
			path: TypePath[named: "$Array"]
			type: Type[
				array: Type[path: TypePath[named: "Elem"]]
			]
		]]
		DeclStmt[typeDecl: Alias[
			template: Maybe[the: Template[
				types: #[
					TypeParam[
						name: Maybe[the: "Elem"]
					]
					TypeParam[
						type: Maybe[the: Type[int]]
						name: Maybe[the: "Size"]
					]
				]
			]]
			path: TypePath[named: "$ArrayN"]
			type: Type[
				array: Type[path: TypePath[named: "Elem"]]
				size: Expr[name: "Size"]
			]
		]]
		DeclStmt[typeDecl: Struct[
			template: Maybe[the: Template[
				types: #[
					TypeParam[
						type: Type[path: TypePath[named: "$Array" of: #[Type[const: Type[char]]]]]
						name: Maybe[the: "Name"]
					]
				]
			]]
			path: TypePath[named: "$L"]
		]]
		DeclStmt[typeDecl: Struct[
			template: Maybe[the: Template[
				types: #[
					TypeParam[
						name: Maybe[the: "T"]
					]
				]
			]]
			path: TypePath[named: "$T"]
			body: ClassBody[
				normal: DeclBody #[
					DeclStmt[typeDecl: Alias[
						path: TypePath[named: "Type"]
						type: Type[path: TypePath[named: "T"]]
					]]
				]
			]
		]]
	]
	
	on [getFullPath: lookup (Typer.LookupType)] (TypePath) {
		match lookup {
			at Typer.File[dir: my unit (Typer.Unit)] {
				my names = TypePath #[]
				
				while true {
					names[add: #{unit.name, Maybe[none]}]
					
					match unit.outer at my unit' (Typer.Unit) {
						unit = unit'
					} else {
						break
					}
				}
				
				return names
			}
			
			at Typer.File[dir: _] => return TypePath #[]
			
			at my type (Typer.TypeDecl) {
				return this[getFullPath: type.lookup]
				-> [add: #{
					type.name.name
					type.params[collect: $0[collect: Type[fromType: this, $.0]]]
				}]
			}
			
			else => throw "error!"
		}
	}
}