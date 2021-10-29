type K
type V
category Unsafe for Dict[K, V] {
	on [atNew: key (K) set: value (V)] is setter {
		pairs[add: Pair #{key, value}]
	}
}