type T
class Value[T] of HasPrev[T], HasNext[T] is hidden List is sealed {
	my value (T)
}