type T
protocol Iterable[T] {
	on [Iterator[T]]
}

type K
type V
alias Iterable[K, V] = Iterable[Tuple[K, V]]