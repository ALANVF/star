module Strong {
	alias A (Int)

	alias B (A) {
		on [abs] (This) {
			return -this[A][abs]
		}
	}

	type T
	alias C[T] (T)

	type T {
		on [Str]
	}
	alias Stringy (T)
}