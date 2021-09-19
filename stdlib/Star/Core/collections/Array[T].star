use Native

type T
class Array[T] of Values[T] {
	my buffer is hidden
	my length is getter
	my capacity is getter


	;== Creating (macros)

	init [_: values (Array[T])] is macro {
		#init_this This[new: #expand values.length]

		for my value in: values {
			this[add: #expand value]
		}
	}


	;== Collecting

	type U
	on [collect: func (Func[U, T, Int])] (This[U]) {
		return This[U][new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: func[call: _.buffer[at: i], i]]
			}
		}
	}

	type U
	type V of Iterable[U]
	on [collectAll: func (Func[V, T, Int])] (This[U]) {
		return This[U][new: length] -> {
			for my i from: 0 upto: _.length {
				this[addAll: func[call: _.buffer[at: i], i]]
			}
		}
	}


	;== Collecting *and* filtering

	type U
	on [collectIf: func (Func[Maybe[U], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				}
			}
		}
	}

	type U
	on [collectWhile: func (Func[Maybe[U], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				} else {
					break
				}
			}
		}
	}

	type U
	type V of Iterable[U]
	on [collectAllIf: func (Func[Maybe[V], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				}
			}
		}
	}

	type U
	type V of Iterable[U]
	on [collectAllWhile: func (Func[Maybe[V], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				} else {
					break
				}
			}
		}
	}


	;== Chunking

	on [
		every: size (Int)
		by: offset (Int) = 0
		allowPartial: (Bool) = true
	] (This[This]) {
		if size <= 0 {
			throw "Invalid size"
		}

		if offset + size <= 0 {
			throw "Invalid offset"
		}

		;-- Thanks to @somebody1234 for helping me figure out this algorithm

		;[
		==| Whether to add 1: Does the size of 1..size-1 equal size - 1?
		==|
		==| |--------------------------------------------------------------|
		==| | length % (size + offset) |     status     | whether to add 1 |
		==| |--------------------------|----------------|------------------|
		==| | 0                        | chunk finished | no               |
		==| | 1..size-1                | partial chunk  | yes              |
		==| | size..size+offset        | current chunk  | no               |
		==|
		==| |--------------------------------------------------------------------|
		==| | (length - 1) % (size + offset) |     status     | whether to add 1 |
		==| |--------------------------------|----------------|------------------|
		==| | 1..size-1                      | partial chunk  | yes              |
		==| | size..size+offset              | current chunk  | no               |
		==| | 0                              | chunk finished | no               |
		]

		my partialOffset = {
			my shouldAdd1 = (length - 1) % (size + by) < size - 1
			
			if allowPartial && shouldAdd1 {
				return 1
			} else {
				return 0
			}
		}
		my newSize = (length + by) // (size + by) + partialOffset
		my result = This[This][new: newSize]
		my i = 0

		while i + size <= length {
			result[add: this[from: i by: size]]
			i += size + by
		}

		if allowPartial && i < length {
			result[add: this[from: i]]
		}

		return result
	}
	
	
	;== Converting
	
	type T' if Power.Castable[T, T']?
	on [Array[T']] {
		my result = Array[new: length]
		
		for my i from: 0 upto: length {
			result[add: buffer[at: i][T']]
		}
		
		return result
	}
	
	on [Series[T]] is inline {
		return Series[new: this]
	}
	
	on [Dict[Int, T]] {
		my result = Dict[new: length]
		
		for my i from: 0 upto: length {
			result[Unsafe atNew: i] = buffer[at: i]
		}
		
		return result
	}
}

type T
type U of Collection[T]
class Array[U] {
	on [flatten] (This[T]) {
		my newLength = 0

		for my i from: 0 upto: length {
			newLength += buffer[at: i].length
		}

		my result = This[T][new: newLength]

		for my i from: 0 upto: length {
			result[addAll: buffer[at: i]]
		}

		return result
	}
}

type T
type U of Iterable[T]
class Array[U] {
	on [flatten] (This[T]) {
		my result = This[T][new: length]

		for my i from: 0 upto: length {
			result[addAll: buffer[at: i]]
		}

		return result
	}
}