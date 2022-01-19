type T
protocol Iterable[T] {
	on [Iterator[T]]
}

type K
type V
protocol Iterable[K, V] {
	on [Iterator[Tuple[K, V]]]
}