use Ident from: Parser

alias TypeVars = MultiDict[Str, TypeVar]

class TypeVar of AnyTypeDecl {
	my ident (Ident) is getter
	my params (Array[Type])
	
	my parents (Array[Type])
	
	my rule (Maybe[TypeRule])
	
	my defaultInit (Maybe[DefaultInit])
	my deinit (Maybe[Deinit])
	my inits (Array[Init]) is getter
	my members (Array[Member]) is getter
	my methods (Array[RealMethod]) is getter
	my operators (Array[Operator]) is getter
	my staticInit (Maybe[StaticInit])
	my staticDeinit (Maybe[StaticDeinit])
	my staticMembers (Array[Member]) is getter
	my staticMethods (Array[StaticMethod]) is getter
	my taggedCases (Array[TaggedCase]) is getter
	my valueCases (Array[ValueCase]) is getter
	my categories (Array[Category]) is getter
	
	my native (Maybe[Native])
	my isFlags (Bool)
	my isStrong (Bool)
	my isUncounted (Bool)

	init {
		defaultInit = Maybe[none]
		deinit = Maybe[none]
		inits = #[]
		members = #[]
		methods = #[]
		operators = #[]
		staticInit = Maybe[none]
		staticDeinit = Maybe[none]
		staticMembers = #[]
		staticMethods = #[]
		taggedCases = #[]
		valueCases = #[]
		categories = #[]
		native = Maybe[none]
		isFlags = false
		isStrong = false
		isUncounted = false

		thisType = Type[typevar: this]
	}


	on [name] (Str) is getter => return ident.name
}