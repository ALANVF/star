type K
type V
category Unsafe for Mapped[K, V] {
	on [atNew: (K) set: (V)] is setter {
		this[:atNew :set]
	}
}