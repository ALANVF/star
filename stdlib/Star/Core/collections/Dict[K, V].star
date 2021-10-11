type K
type V
class DictIterator[K] of Iterator[K] is hidden {
	my keys (Array[K])
	my index (Int) = 0
	
	on [next] (Maybe[K]) is inline {
		if index < keys.length {
			return Maybe[the: keys[Unsafe at: index++]]
		} else {
			return Maybe[none]
		}
	}
}

type K
type V
class DictIterator[K, V] of Iterator[K, V] is hidden {
	my pairs (Array[Tuple[K, V]])
	my index (Int) = 0
	
	on [next] (Maybe[Tuple[K, V]]) is inline {
		if index < pairs.length {
			return Maybe[the: pairs[Unsafe at: index++]]
		} else {
			return Maybe[none]
		}
	}
}

type K
type V
class Dict[K, V] of Mapped[K, V] is friend #[DictIterator[K], DictIterator[K, V]] {
	class Pair[K, V] is hidden {
		my key (K)
		my value (V)
	}
	
	
	my pairs (Array[Pair]) is hidden
	
	
	;== Creating (macros)

	init [_: entries (Dict[K, V])] is macro {
		my entries' = #quote Array[Pair] #[]

		for my key, my value in: entries {
			entries'[add: #quote Pair #{#expand key, #expand value}]
		}

		pairs = #expand entries'
	}
	
	
	;== Creating
	
	init [new] {
		pairs = #[]
	}
	
	init [new: capacity (Int)] {
		pairs = Array[new: capacity]
	}
	
	init [new: pairs (Array[Tuple[K, V]])] {
		this.pairs = pairs[Array[Pair]]
	}
	
	
	;== Copying
	
	on [new] (This) => return This[pairs: pairs[collect: $0[new]]]
	
	
	;== Sizes
	
	on [size] (Int) is getter is inline => return pairs.length
	
	on [capacity] (Int) is getter is inline => return pairs.capacity
	
	
	;== Keys
	
	on [keys] (Array[K]) is getter => return pairs[collect: $0.key]
	
	
	;== Values
	
	on [values] (Array[V]) is getter => return pairs[collect: $0.value]
	
	
	;== Pairs
	
	on [pairs] (Array[Tuple[K, V]]) is getter => return pairs[Array[Tuple[K, V]]]
	
	
	;== Internal
	
	on [indexForKey: key (K)] (Maybe[Int]) is hidden is inline {
		for my i from: 0 upto: pairs.length {
			if pairs[Unsafe at: i].key ?= key {
				return Maybe[the: i]
			}
		}
		
		return Maybe[none]
	}
	
	on [pairForKey: key (K)] (Maybe[Pair]) is hidden is inline {
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if pair.key ?= key {
				return Maybe[the: pair]
			}
		}
		
		return Maybe[none]
	}
	
	on [indexForValue: value (V)] (Maybe[Int]) is hidden is inline {
		for my i from: 0 upto: pairs.length {
			if pairs[Unsafe at: i].value ?= value {
				return Maybe[the: i]
			}
		}
		
		return Maybe[none]
	}
	
	
	;== Accessing
	
	on [at: key (K)] (V) {
		match this[pairForKey: key] at Maybe[the: Pair[value: my value]] {
			return value
		} else {
			throw KeyError[new]
		}
	}
	
	on [at: key (K) set: value (V)] is setter {
		match this[pairForKey: key] at Maybe[the: my pair] {
			pair.value = value
		} else {
			pairs[add: Pair #{key, value}]
		}
	}
	
	
	on [maybeAt: key (K)] (V) {
		match this[pairForKey: key] at Maybe[the: Pair[value: my value]] {
			return Maybe[the: value]
		} else {
			return Maybe[none]
		}
	}
	
	on [maybeAt: key (K) set: value (V)] is setter {
		match this[pairForKey: key] at Maybe[the: my pair] {
			pair.value = value
		}
	}
	
	
	on [atNew: key (K) set: value (V)] is setter {
		match this[pairForKey: key] at Maybe[none] {
			pairs[add: Pair #{key, value}]
		}
	}

	on [atExisting: key (K) set: value (V)] is setter {
		match this[pairForKey: key] at Maybe[the: my pair] {
			pair.value = value
		}
	}
	
	
	;== Removing by key
	
	on [removeKey: key (K)] (V) {
		match this[indexForKey: key] at Maybe[the: my index] {
			return pairs[Unsafe removeAt: index].value
		} else {
			throw KeyError[new]
		}
	}
	
	on [maybeRemoveKey: key (K)] (Maybe[V]) {
		match this[indexForKey: key] at Maybe[the: my index] {
			return Maybe[the: pairs[Unsafe removeAt: index].value]
		} else {
			return Maybe[none]
		}
	}
	
	
	;== Removing by value
	
	on [removeValue: value (V)] (K) {
		match this[indexForValue: value] at Maybe[the: my index] {
			return pairs[Unsafe removeAt: index].key
		} else {
			throw NotFound[new]
		}
	}
	
	on [maybeRemoveValue: value (V)] (K) {
		match this[indexForValue: value] at Maybe[the: my index] {
			return Maybe[the: pairs[Unsafe removeAt: index].key]
		} else {
			return Maybe[none]
		}
	}
	
	
	;== Clearing
	
	on [clear] is inline => pairs[clear]
	
	
	;== Collecting
	
	type K'
	type V'
	on [collect: func (Func[Tuple[K', V'], K, V])] (This[K', V']) {
		my result = This[K', V'][new: pairs.capacity]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			#{my key, my value} = func[call: pair.key, pair.value]

			result[at: key] = value
		}
		
		return result
	}
	
	
	;== Collecting *and* filtering
	
	type K'
	type V'
	on [collectIf: func (Func[Maybe[Tuple[K', V']], K, V])] (This[K', V']) {
		my result = This[K', V'][new: pairs.capacity // 2]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			match func[call: pair.key, pair.value] at Maybe[the: #{my key, my value}] {
				result[at: key] = value
			}
		}
		
		return result
	}
	
	type K'
	type V'
	on [collectWhile: func (Func[Maybe[Tuple[K', V']], K, V])] (This[K', V']) {
		my result = This[K', V'][new: pairs.capacity // 2]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			match func[call: pair.key, pair.value] at Maybe[the: #{my key, my value}] {
				result[at: key] = value
			} else {
				break
			}
		}
		
		return result
	}
	
	
	;== Iterating
	
	on [Iterator[K]] is inline => return DictIterator[K][keys: this.keys]
	on [Iterator[K, V]] is inline => return DictIterator[K, V][pairs: this[pairs]]
}