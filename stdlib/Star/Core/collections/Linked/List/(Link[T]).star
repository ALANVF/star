type T
protocol Link[T] is hidden List is sealed List {
	on [Immutable.List[T]] is inline {
		match this {
			at Head[next: my link] => return link[Immutable.List[T]]
			at Value[value: my value next: my link] => return Immutable.List[head: value tail: link[Immutable.List[T]]]
			at Tail => return Immutable.List[nil]
		}
	}
}