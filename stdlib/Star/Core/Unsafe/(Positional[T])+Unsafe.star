type T
category Unsafe for Positional[T] {
	on [at: (Int)] (T) => return this[:at]

	on [at: (Int) set: (T)] is setter => this[:at :set]

	on [removeAt: (Int)] (T) => return this[:removeAt]
}