;[
==| The EmptyIterator class is a type of iterator that never provides a value.
]

type T
class EmptyIterator[T] of Iterator[T] {
	on [next] (Maybe[T]) {
		return Maybe[none]
	}
}