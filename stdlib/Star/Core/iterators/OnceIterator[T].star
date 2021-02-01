;[
==| The Onceterator class is a type of iterator that only provides one value.
]

type E
class OnceIterator[T] of Iterator[T] {
	my value (Maybe[T]) is hidden

	init [new: value (T)] {
		this.value = Maybe[the: value]
	}

	on [next] (Maybe[T]) {
		my result = value

		if value? {
			value = Maybe[none]
		}

		return result
	}
}