type T
protocol Link[T] is hidden List is sealed List {
	on [Immutable.List[T]] is inline {
		match this {
			;at Head[T][next: my link] => return link[Immutable.List[T]]
			at my head (Head[T]) => return head.next[Immutable.List[T]]
			;at Value[T][value: my value next: my link] => return Immutable.List[head: value tail: link[Immutable.List[T]]]
			at my value (Value[T]) => return Immutable.List[head: value.value tail: value.next[Immutable.List[T]]]
			at Tail[T] => return Immutable.List[nil]
		}
	}
}