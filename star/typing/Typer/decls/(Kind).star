protocol Kind of ClassLike {
	my deinit (Maybe[Deinit]) = Maybe[none]
	my isFlags (Bool) = false
	my isStrong (Bool) = false
	my isUncounted (Bool) = false
}