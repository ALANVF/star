class ValueKind of Kind {
	my repr (Maybe[Type]) = Maybe[none]
	my valueCases (Array[ValueCase]) is getter = #[]
}