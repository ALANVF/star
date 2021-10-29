type T
category Unsafe for Maybe[T] {
	on [value] (T) is getter is inline is asm {
		return [#kind_slot #{this, 0} T]
	}

	on [length] (Int) is getter is inline is asm {
		return [#kind_id this Int]
	}
}