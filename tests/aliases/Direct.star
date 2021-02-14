module Direct {
	alias A = Int

	alias B = A

	type T
	alias C[T] = T

	type T {
		on [Str]
	}
	alias Stringy = T
}