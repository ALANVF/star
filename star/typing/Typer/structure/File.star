class File of TypeLookup {
	my dir (Dir) is getter
	my path (Str) is getter
	my source (Maybe[SourceFile]) = Maybe[none]
	my program (Maybe[Parser.Program]) = Maybe[none]
	my status (Bool) = false
	;my imports (Array[Import]) is getter = #[]
	;my imported (Array[TImport]) is getter = #[]
	my decls (MultiDict[Str, TypeDecl]) is getter = MultiDict #()
	my sortedDecls (Array[TypeDecl]) is getter = #[]
	my categories (Array[Category]) is getter = #[]
}