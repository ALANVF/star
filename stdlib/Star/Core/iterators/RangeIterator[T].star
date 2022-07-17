type T of Ordered
class RangeIterator[T] of Iterator[T] {
	my from (T)
	my to (T)
	my value (T) = to

	on [next] (Maybe[T]) {
		if value ?= to {
			return Maybe[none]
		} else {
			return Maybe[the: value++]
		}
	}
}