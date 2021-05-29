type T
protocol Iterator[T] {
	on [next] (Maybe[T])
}