protocol Ordered of Comparable {
    on [next] (This)
    on [previous] (This)

	operator `++` (This) is inline {
		return this[next]
	}

	operator `--` (This) is inline {
		return this[previous]
	}
}