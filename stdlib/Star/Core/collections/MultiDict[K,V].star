type K
type V
class MultiDict[K, V] {
	; ...

	on [values] (Array[V]) is getter

	; ...

	on [ad: key (K)] (Array[V])

	on [at: key (K) add: value (V)] (V)

	; ...
}