class Protocol of ClassLike {
	my inits (Array[Init]) is getter = #[]
	my defaultInit (Maybe[DefaultInit]) = Maybe[none]
	my deinit (Maybe[Deinit]) = Maybe[none]
}