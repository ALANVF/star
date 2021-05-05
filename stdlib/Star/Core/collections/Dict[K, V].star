type K
class DictIterator[K] of Iterator[K] is hidden {
	my pairs (Array[Tuple[K, _]])
	my index (Int) = 0
	
	on [next] (Maybe[K]) {
		if index < pairs.length {
			return Maybe[the: pairs[Unsafe at: index++].key]
		} else {
			return Maybe[none]
		}
	}
}

type K
type V
class DictIterator[K, V] of Iterator[K, V] is hidden {
	my pairs (Array[Dict[K, V].Pair])
	my index (Int) = 0
	
	on [next] (Maybe[Tuple[K, V]]) {
		if index < pairs.length {
			return Maybe[the: pairs[Unsafe at: index++][Tuple[K, V]]]
		} else {
			return Maybe[none]
		}
	}
}

type K
type V
class Dict[K, V] of Iterable[K], Iterable[K, V] is friend #[DictIterator[K], DictIterator[K, V]] {
	type K'
	type V'
	alias This'[K', V'] is hidden = Power.Reapply[This, K', V']
	
	class Pair is hidden is strong {
		my key (K)
		my value (V)
		
		on [Tuple[K, V]] is inline {
			return #{key, value}
		}
	}
	
	
	my pairs (Array[Pair]) is hidden
	
	
	;== Creating
	
	init [new] {
		pairs = #[]
	}
	
	init [new: capacity (Int)] {
		pairs = Array[new: capacity]
	}
	
	init [new: pairs (Array[Tuple[K, V]])] {
		this.pairs = pairs[collect: Pair[key: $.0.first value: $.0.second]]
	}
	
	
	;== Copying
	
	on [new] (This) {
		return This[pairs: pairs[collect: $0[new]]]
	}
	
	
	;== Sizes
	
	on [size] (Int) is getter is inline {
		return pairs.length
	}
	
	on [capacity] (Int) is getter is inline {
		return pairs.capacity
	}
	
	
	;== Keys
	
	on [keys] (Array[K]) is getter {
		return pairs[collect: $0.key]
	}
	
	
	;== Values
	
	on [values] (Array[V]) is getter {
		return pairs[collect: $0.value]
	}
	
	
	;== Pairs
	
	on [pairs] (Array[Tuple[K, V]]) is getter {
		return pairs[collect: $0[Tuple[K, V]]]
	}
	
	
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
			pairs[add: Pair[:key :value]]
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
			pairs[add: Pair[:key :value]]
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
	
	on [removeAllKeys: keys (Iterable[K])] (Array[V]) {
		my values = #[]
		
		for my key in: keys {
			values[add: this[removeKey: key]]
		}
		
		return values
	}
	
	on [removeAnyKeys: keys (Iterable[K])] (Array[V]) {
		my values = #[]
		
		for my key in: keys {
			match this[maybeRemoveKey: key] at Maybe[the: my value] {
				values[add: value]
			}
		}
		
		return values
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
	
	on [removeAllValues: values (Iterable[V])] (Array[K]) {
		my keys = #[]
		
		for my value in: values {
			keys[add: this[removeValue: value]]
		}
		
		return keys
	}
	
	on [removeAnyValues: values (Iterable[V])] (Array[K]) {
		my keys = #[]
		
		for my value in: values {
			match this[maybeRemoveValue: value] at Maybe[the: my key] {
				keys[add: key]
			}
		}
		
		return keys
	}
	
	
	;== Clearing
	
	on [clear] is inline {
		pairs[clear]
	}
	
	
	;== Collecting
	
	type K'
	type V'
	on [collect: func (Func[Tuple[K', V'], K, V])] (This'[K', V']) {
		my result = This'[K', V'][new: pairs.capacity]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			match func[call: pair.key, pair.value] at #{my key, my value} {
				result[at: key] = value
			}
		}
		
		return result
	}
	
	
	;== Filtering
	
	on [keepIf: func (Func[Bool, K, V])] (This) {
		my result = This[new: pairs.capacity // 2]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
				result[add: pair.key] = pair.value
			}
		}
		
		return result
	}
	
	on [keepWhile: func (Func[Bool, K, V])] (This) {
		my result = This[new: pairs.capacity // 2]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
				result[add: pair.key] = pair.value
			} else {
				break
			}
		}
		
		return result
	}
	
	
	;== Collecting *and* filtering
	
	type K'
	type V'
	on [collectIf: func (Func[Maybe[Tuple[K', V']], K, V])] (This'[K', V']) {
		my result = This'[K', V'][new: pairs.capacity // 2]
		
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
	on [collectWhile: func (Func[Maybe[Tuple[K', V']], K, V])] (This'[K', V']) {
		my result = This'[K', V'][new: pairs.capacity // 2]
		
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
	
	
	;== Observing

	on [all: func (Func[Bool, K, V])] (Bool) {
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if !func[call: pair.key, pair.value] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, K, V])] (Bool) {
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, K, V])] (Bool) {
		my cond = false

		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
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
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
				return false
			}
		}

		return true
	}

	on [containsKey: key (K)] (Bool) {
		for my i from: 0 upto: pairs.length {
			if pairs[Unsafe at: i].key ?= key {
				return true
			}
		}

		return false
	}

	on [containsValue: value (V)] (Bool) {
		for my i from: 0 upto: pairs.length {
			if pairs[Unsafe at: i].value ?= value {
				return true
			}
		}

		return false
	}
	
	
	;== Iterating
	
	on [each: func (Func[Void, K, V])] {
		for my i from: 0 upto: pairs.length {
			pairs[Unsafe at: i] -> {
				func[call: key, value]
			}
		}
	}
	
	on [Iterator[K]] is inline {
		return DictIterator[K][:pairs]
	}
	
	on [Iterator[K, V]] is inline {
		return DictIterator[K, V][:pairs]
	}
	
	
	;== Converting
	
	on [Array[Tuple[K, V]]] is inline {
		return this.pairs
	}
	
	type K' if K' of K || K of K'
	type V' if V' of V || V of V'
	on [Dict[K', V']] {
		my result = Dict[K', V'][new: pairs.capacity]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			result[at: pair.key[K']] = pair.value[V']
		}
		
		return result
	}
	
	
	;== Merging
	
	operator `&` [other (This)] (This) {
		my result = This[new]
		
		for my i from: 0 upto: pairs.length {
			my key = pairs[Unsafe at: i].key
			
			match other[maybeAt: key] at Maybe[the: my value] {
				result[atNew: key] = value
			}
		}
		
		return result
	}
	
	operator `|` [other (This)] (This) {
		my result = this[new]
		
		for my key, my value in: other {
			result[atNew: key] = value
		}
		
		return result
	}
	
	operator `^` [other (This)] (This) {
		my result = This[new]
		
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if !other[containsKey: pair.key] {
				result[atNew: pair.key] = pair.value
			}
		}
		
		for my key, my value in: other {
			if !this[containsKey: key] {
				result[atNew: key] = value
			}
		}
	}
}