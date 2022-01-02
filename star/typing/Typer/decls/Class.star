class Class of ClassLike {
	my inits (Array[Init]) is getter = #[]
	my defaultInit (Maybe[DefaultInit]) = Maybe[none]
	my deinit (Maybe[Deinit]) = Maybe[none]
	my native (Maybe[Native]) = Maybe[none]
	my isStrong (Bool) = false
	my isUncounted (Bool) = false
}