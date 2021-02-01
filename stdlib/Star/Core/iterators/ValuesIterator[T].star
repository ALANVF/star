use Native

type T
class ValuesIterator[T] of Iterator[T] {
	my values (Values[T]) is readonly
	my index (Int) is getter


	;== Creation

	init [new: values (Values[T])] {
		this.values = value[new]
		index = 0
	}


	;== Iteration

	on [next] (Maybe[T]) {
		if index < values.length {
			return Maybe[the: values[Unsafe at: index++]]
		} else {
			return Maybe[none]
		}
	}
}