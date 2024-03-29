type T
protocol HasNext[T] of Link[T] is hidden List is sealed List {
	my next (HasPrev[T]) is getter is setter `rawNext`

	on [nextValue] (Value[T]) is getter is inline => return this.next[Unsafe Value[T]]

	on [next: newNext (HasPrev[T])] is setter {
		this.next = newNext
		-> rawPrev = this
	}

	on [insertNext: newNext (Value[T])] {
		newNext
		-> next = this.next
		-> prev = this
	}
}