type K
type V
class MultiDict[K, V] {
	; ...

	on [values] (Array[V]) is getter

	; ...

	on [at: key (K)] (Array[V])
	on [at: key (K) add: value (V)] (V)

	on [maybeAt: key (K)] (Maybe[Array[V]])

	; ...
}