type T
protocol HasNext[T] of Link[T] is hidden List is sealed List {
	my next (HasPrev[T]) is setter `rawNext`

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