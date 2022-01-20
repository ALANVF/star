use Native

type T
class ValuesIterator[T] of Iterator[T] {
	my values (Values[T]) is readonly
	my index (Int) is getter


	;== Creation

	init [new: values (Values[T])] {
		_.values = values[new]
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


type T
category Unsafe for ValuesIterator[T] {
	init [newWithoutCopying: values (Values[T])] {
		_.values = values
		index = 0
	}
}