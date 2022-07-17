type K
type V
protocol Mapped[K, V] of Iterable[K], Iterable[K, V] {
	;== Creating

	init [new: capacity (Int)] => #init_this This[new]


	;== Sizes
	
	on [size] (Int) is getter


	;== Keys
	
	on [keys] (Array[K]) is getter
	
	
	;== Values
	
	on [values] (Array[V]) is getter
	
	
	;== Entries
	
	on [entries] (Array[Tuple[K, V]]) is getter


	;== Accessing
	
	on [at: key (K)] (V)
	on [at: key (K) set: value (V)] is setter
	
	on [maybeAt: key (K)] (Maybe[V])
	on [maybeAt: key (K) set: value (V)] is setter
	
	on [atNew: key (K) set: value (V)] is setter
	on [atExisting: key (K) set: value (V)] is setter


	;== Removing by key
	
	on [removeKey: key (K)] (V)
	
	on [maybeRemoveKey: key (K)] (Maybe[V])
	
	on [removeAllKeys: keys (Iterable[K])] (Array[V]) {
		my values = Array[V] #[]
		
		for my key in: keys {
			values[add: this[removeKey: key]]
		}
		
		return values
	}

	on [removeAnyKeys: keys (Iterable[K])] (Array[V]) {
		my values = Array[V] #[]
		
		for my key in: keys {
			match this[maybeRemoveKey: key] at Maybe[the: my value] {
				values[add: value]
			}
		}
		
		return values
	}
	
	
	;== Removing by value
	
	on [removeValue: value (V)] (K)
	
	on [maybeRemoveValue: value (V)] (K)
	
	on [removeAllValues: values (Iterable[V])] (Array[K]) {
		my keys = Array[K] #[]
		
		for my value in: values {
			keys[add: this[removeValue: value]]
		}
		
		return keys
	}
	
	on [removeAnyValues: values (Iterable[V])] (Array[K]) {
		my keys = Array[K] #[]
		
		for my value in: values {
			match this[maybeRemoveValue: value] at Maybe[the: my key] {
				keys[add: key]
			}
		}
		
		return keys
	}
	
	
	;== Clearing
	
	on [clear]


	;== Filtering
	
	on [keepIf: func (Func[Bool, K, V])] (This) {
		my result = This[new: this.size // 2]
		
		for my key, my value in: this {
			if func[call: key, value] {
				result[atNew: key] = value
			}
		}
		
		return result
	}
	
	on [keepWhile: func (Func[Bool, K, V])] (This) {
		my result = This[new: this.size // 2]
		
		for my key, my value in: this while: func[call: key, value] {
			result[Unsafe atNew: key] = value
		}
		
		return result
	}


	;== Observing

	on [all: func (Func[Bool, K, V])] (Bool) {
		for my key, my value in: this {
			if !func[call: key, value] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, K, V])] (Bool) {
		for my key, my value in: this {
			if func[call: key, value] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, K, V])] (Bool) {
		my cond = false

		for my key, my value in: this {
			if func[call: key, value] {
				if cond {
					return false
				} else {
					cond = true
				}
			}
		}

		return cond
	}

	on [none: func (Func[Bool, K, V])] (Bool) {
		for my key, my value in: this {
			if func[call: key, value] {
				return false
			}
		}

		return true
	}

	on [containsKey: key (K)] (Bool) {
		for my k in: this {
			if k ?= key {
				return true
			}
		}

		return false
	}

	on [containsValue: value (V)] (Bool) {
		for _, my v in: this {
			if v ?= value {
				return true
			}
		}

		return false
	}


	;== Iterating
	
	on [each: func (Func[Void, K, V])] is inline {
		for my key, my value in: this {
			func[call: key, value]
		}
	}
	
	on [Iterator[K]]
	on [Iterator[K, V]]


	;== Converting
	
	on [Array[Tuple[K, V]]] is inline => return this.entries
	
	type K' if Power.Castable[K, K']?
	type V' if Power.Castable[V, V']?
	on [This[K', V']] {
		my result = This[K', V'][new: this.size]
		
		for my key, my value in: this {
			result[Unsafe atNew: key[K']] = value[V']
		}
		
		return result
	}

	on [invert] (This[V, K]) {
		my result = This[V, K][new]

		for my key, my value in: this {
			result[at: value] = key
		}

		return result
	}
	
	
	;== Merging

	operator `+` [other (This)] (This) {
		my result = this[new]
		
		for my key, my value in: other {
			result[atNew: key] = value
		}
		
		return result
	}

	operator `-` [other (This)] (This) {
		my result = This[new: this.size]
		
		for my key, my value in: this {
			if !other[containsKey: key] {
				result[Unsafe atNew: key] = value
			}
		}
		
		return result
	}
	
	operator `&` [other (This)] (This) {
		my result = This[new]
		
		for my key, my value in: this {
			if other[containsKey: key] {
				result[Unsafe atNew: key] = value
			}
		}
		
		return result
	}
	
	operator `|` [other (This)] (This) {
		my result = this[new]
		
		for my key, my value in: other {
			result[at: key] = value
		}
		
		return result
	}
	
	operator `^` [other (This)] (This) {
		my result = This[new]
		
		for my key, my value in: this {
			if !other[containsKey: key] {
				result[atNew: key] = value
			}
		}
		
		for my key, my value in: other {
			if !this[containsKey: key] {
				result[atNew: key] = value
			}
		}
	}
}


type K
type V
category Unsafe for Mapped[K, V] {
	on [atNew: (K) set: (V)] is setter {
		this[:atNew :set]
	}
}