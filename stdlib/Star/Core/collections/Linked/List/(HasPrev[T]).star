type T
protocol HasPrev[T] of Link[T] is hidden List is sealed List {
	my prev (HasNext[T]) is getter is setter `rawPrev`

	on [prevValue] (Value[T]) is getter is inline => return prev[Unsafe Value[T]]

	on [prev: newPrev (HasNext[T])] is setter {
		prev = newPrev
		-> rawNext = this
	}

	on [insertPrev: newPrev (Value[T])] {
		newPrev
		-> prev = this.prev
		-> next = this
	}
}