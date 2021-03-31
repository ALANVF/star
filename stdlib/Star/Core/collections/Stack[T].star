type T
class Stack[T] of Array[T] ;[is noinherit Array[T]] {
	on [push: value (T)] (T) is inline {
		return this[prepend: value]
	}
	
	on [pop] (T) is inline {
		return this[removeAt: 0]
	}
	
	on [maybePop] (Maybe[T]) is inline {
		return this[maybeRemoveAt: 0]
	}
	
	on [peek] (T) is inline {
		return this[at: 0]
	}
	
	on [maybePeek] (Maybe[T]) is inline {
		return this[maybeAt: 0]
	}
}