class TaggedKind of Kind {
	my taggedCases (Array[TaggedCase]) is getter = #[]
	my defaultInit (Maybe[DefaultInit]) = Maybe[none]
}