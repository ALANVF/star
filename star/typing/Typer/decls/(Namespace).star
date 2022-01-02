protocol Namespace of TypeDecl {
	my parents (Array[Type]) is getter = #[]

	my decls (MultiDict[Str, TypeDecl]) is getter = MultiDict #()
	my categories (Array[Category]) is getter = #[]
	
	my staticMembers (Array[Member]) is getter = #[]
	my staticMethods (Array[StaticMethod]) is getter = #[]
	my staticInit (Maybe[StaticInit]) = Maybe[none]
	my staticDeinit (Maybe[StaticDeinit]) = Maybe[none]

	my sealed (Maybe[Maybe[Type]]) = Maybe[none]
}