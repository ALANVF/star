class Category of AnyTypeDecl {
	my typevars (MultiDict[Str, TypeVar]) is getter = MultiDict #()
	my path (Type)
	my type (Maybe[Type])
	my staticMembers (Array[Member]) is getter = #[]
	my staticMethods (Array[StaticMethod]) is getter = #[]
	my methods (Array[Method]) is getter = #[]
	my inits (Array[Init]) is getter = #[]
	my operators (Array[Operator]) is getter = #[]
	my hidden (Maybe[Maybe[Type]]) = Maybe[none]
	my friends (Array[Type]) is getter = #[]
}