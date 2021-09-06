type T
protocol Iterator[T] {
	on [next] (Maybe[T])
}

type K
type V
alias Iterator[K, V] = Iterator[Tuple[K, V]]