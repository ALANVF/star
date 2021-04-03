kind Program {
	my errors (Array[Diagnostic])
	
	has [modular: decls (Array[Decl])]
	has [script: decls (Array[ScriptDecl])]
}