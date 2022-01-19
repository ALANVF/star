type K
type V
kind Dict[K, V] of Iterable[K, V] {
	; ...

	on [new] (This) is static

	on [contains: key (K)] (Bool)

	on [at: key (K)] (V)
	on [at: key (K) set: value (V)] (This)

	on [maybeAt: key (K)] (Maybe[V])
	on [maybeAt: key (K) set: value (V)] (This)

	; ...

	on [Iterator[K, V]]

	; ...

	on [_.Dict[K, V]]

	; ...
}