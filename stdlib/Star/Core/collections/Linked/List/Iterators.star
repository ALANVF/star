type T
class ListIterator[T] of Iterator[T] is hidden List {
	my link (HasPrev[T])
	my tail (Tail[T])

	on [next] (Maybe[T]) is inline {
		if link ?= tail {
			return Maybe[none]
		} else {
			Value[value: my value next: link] = link[Unsafe Value[T]]
			return value
		}
	}
}

type T
class LinkIterator[T] of Iterator[Value[T]] is hidden List {
	my link (HasNext[T])
	my tail (Tail[T])

	on [next] (Maybe[Value[T]]) is inline {
		my link' = link.next

		if link' ?= tail {
			return Maybe[none]
		} else {
			my vlink = link'[Unsafe Value[T]]
			link = vlink
			return vlink
		}
	}
}