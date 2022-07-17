type T of Ordered
class Range[T] of Iterable[T] {
	my from (T)
	my to (T)

	on [Iterator[T]] is inline => return RangeIterator[T][:from :to]
}