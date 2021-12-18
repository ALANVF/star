class Project of Dir {
	my stdlib (Maybe[Project]) is static = Maybe[none]

	my main (Maybe[File]) = Maybe[none]
	my useStdlib (Bool)
}