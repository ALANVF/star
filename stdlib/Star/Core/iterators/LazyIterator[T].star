;[
==| The LazyIterator class is a type of iterator that lazily provides values.
]

type E
class LazyIterator[T] of Func[Maybe[T]], Iterator[T] {
	on [new: func (Func[Maybe[T]])] (This) is static is inline {
		return func[This]
	}
	
	;[
	==| If it was possible, we'd just have #next and #call point to the same
	==| member function. Since that's not possible (for now), just inline
	==| the call when possible.
	]
	on [next] (Maybe[T]) is inline {
		return this[call]
	}
}