kind Program {
	my errors (Array[Error])
	
	has [modular: decls (Array[Decl])]
	has [script: decls (Array[ScriptDecl])]
}