type T
protocol Iterator[T] of Iterable[T] {
	on [next] (Maybe[T])
}