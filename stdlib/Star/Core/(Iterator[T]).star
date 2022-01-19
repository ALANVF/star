type T
protocol Iterator[T] {
	on [next] (Maybe[T])
}

type K
type V
protocol Iterator[K, V] {
	on [next] (Maybe[Tuple[K, V]])
}