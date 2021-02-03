type T
category Unsafe for ValuesIterator[T] {
	init [newWithoutCopying: values (Values[T])] {
		this.values = values
		index = 0
	}
}