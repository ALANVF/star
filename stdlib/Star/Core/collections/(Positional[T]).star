;@@ TODO: figure out how to include Array#every:?by:?allowPartial: as a generic method

type T
protocol Positional[T] of Collection[T] {
	;== Accessing
	
	on [at: index (Int)] (T)
	on [at: index (Int) set: value (T)] is setter
	
	on [maybeAt: index (Int)] (Maybe[T]) {
		if index < 0 {
			index += length
		}

		if 0 <= index < this.length {
			return Maybe[the: this[Unsafe at: index]]
		} else {
			return Maybe[none]
		}
	}
	on [maybeAt: index (Int) set: value (T)] is setter {
		if index < 0 {
			index += length
		}

		if 0 <= index < this.length {
			this[Unsafe at: index] = value
		}
	}
	
	
	;== Slicing
	
	on [from: (Int)] (This)
	on [from: (Int) set: values (This)] is setter
	
	on [to: (Int)] (This)
	on [to: (Int) set: values (This)] is setter
	
	on [from: (Int) to: (Int)] (This)
	on [from: (Int) to: (Int) set: values (This)] is setter
	
	on [after: (Int)] (This) {
		my from = after + 1
		
		if from ?= this.length {
			return This[new]
		} else {
			return this[from: after + 1]
		}
	}
	on [after: (Int) set: values (This)] is setter {
		my from = after + 1
		
		if from ?= this.length {
			this[addAll: values]
		} else {
			this[:from] = values
		}
	}
	
	on [upto: (Int)] (This) {
		if upto ?= 0 {
			return This[new]
		} else {
			return this[to: upto - 1]
		}
	}
	on [upto: (Int) set: values (This)] is setter {
		if upto ?= 0 {
			this[prependAll: values]
		} else {
			this[to: upto - 1] = values
		}
	}
	
	;on [from: (Int) upto: (Int)] (This)
	;on [from: (Int) upto: (Int) set: values (This)] is setter
	;on [from: (Int) downto: (Int)] (This)
	;on [from: (Int) downto: (Int) set: values (This)] is setter
	;on [from: (Int) by: (Int)] (This)
	;on [from: (Int) by: (Int) set: values (This)] is setter

	;on [after: (Int) to: (Int)] (This)
	;on [after: (Int) to: (Int) set: values (This)] is setter
	;on [after: (Int) upto: (Int)] (This)
	;on [after: (Int) upto: (Int) set: values (This)] is setter
	;on [after: (Int) downto: (Int)] (This)
	;on [after: (Int) downto: (Int) set: values (This)] is setter
	;on [after: (Int) by: (Int)] (This)
	;on [after: (Int) by: (Int) set: values (This)] is setter
	

	;== Prepending
	
	on [prepend: value (T)] (T)
	
	type Pos of Positional[T]
	on [prependAll: values (Pos)] (Pos) {
		for my i after: values.length downto: 0 {
			this[prepend: values[Unsafe at: i]]
		}
		
		return values
	}
	
	type Iter of Iterable[T]
	on [prependAll: values (Iter)] (Iter) {
		my values' = #[]
		-> [addAll: values]
		
		for my i after: values'.length downto: 0 {
			this[prepend: values[Unsafe at: i]]
		}
		
		return values
	}

	
	;== Removing values
	
	;[on [remove: value (T)] (Bool)
	on [remove: value (T) times: (Int)] (Bool)
	
	on [removeWhere: func (Func[Bool, T, Int])] (This)
	
	on [removeLast: value (T)] (Bool)
	on [removeLast: value (T) times: (Int)] (Bool)]

	on [removeAt: index (Int)] (T)
	on [maybeRemoveAt: index (Int)] (Maybe[T]) {
		if index < 0 {
			index += this.length
		}
		
		if 0 <= index < this.length {
			return Maybe[the: this[Unsafe removeAt: index]]
		} else {
			return Maybe[none]
		}
	}
	
	
	;== Removing sections

	on [removeFrom: from (Int)] (This)
	
	on [removeTo: to (Int)] (This)
	
	on [removeFrom: from (Int) to: (Int)] (This)
	
	on [removeAfter: after (Int)] (This) => return this[removeFrom: after + 1]
	
	; ... and all other variants of from/after/to/upto
	
	
	;== Iterating
	
	on [each: func (Func[Void, T, Int])] is inline {
		for my i, my value in: this {
			func[call: value, i]
		}
	}
	
	
	;== Reversing

	on [reverse] (This) {
		match this.length {
			at 0 || 1 => return this[new]
			
			at 2 {
				return This[new: 2]
				-> [add: this[Unsafe at: 1]]
				-> [add: this[Unsafe at: 0]]
			}

			else {
				my result = This[new: this.length]
				
				for my i after: this.length downto: 0 {
					result[add: this[Unsafe at: i]]
				}
				
				return result
			}
		}
	}
	
	
	;== Filtering

	on [keepIf: func (Func[Bool, T, Int])] (This) {
		my result = This[new: this.length // 2]
		
		for my i, my value in: this {
			if func[call: value, i] {
				result[add: value]
			}
		}
		
		return result
	}
	
	on [keepWhile: func (Func[Bool, T, Int])] (This) {
		my result = This[new: this.length // 2]
		
		for my i, my value in: this while: func[call: value, i] {
			result[add: value]
		}
		
		return result
	}
	
	
	;== Observing

	on [all: func (Func[Bool, T, Int])] (Bool) {
		for my i, my value in: this {
			if !func[call: value, i] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, T, Int])] (Bool) {
		for my i, my value in: this {
			if func[call: value, i] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, T, Int])] (Bool) {
		my cond = false

		for my i, my value in: this {
			if func[call: value, i] {
				if cond {
					return false
				} else {
					cond = true
				}
			}
		}

		return cond
	}

	on [none: func (Func[Bool, T, Int])] (Bool) {
		for my i, my value in: this {
			if func[call: value, i] {
				return false
			}
		}

		return true
	}

	on [containsIndex: index (Int)] (Bool) is inline => return 0 <= index < this.length
	
	
	;== Counting

	on [countIf: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i, my value in: this {
			if func[call: value, i] {
				count++
			}
		}

		return count
	}

	on [countWhile: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i, my value in: this while: func[call: value, i] {
			count++
		}

		return count
	}
	
	
	;== Finding

	on [indexOf: value (T)] (Int) {
		for my i, my value' in: this {
			if value' ?= value {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeIndexOf: value (T)] (Int) {
		for my i, my value' in: this {
			if value' ?= value {
				return Maybe[the: i]
			}
		}

		return Maybe[none]
	}

	on [find: func (Func[Bool, T, Int])] (T) {
		for my i, my value in: this {
			if func[call: value, i] {
				return value
			}
		}

		throw NotFound[new]
	}

	on [maybeFind: func (Func[Bool, T, Int])] (Maybe[T]) {
		for my i, my value in: this {
			if func[call: value, i] {
				return Maybe[the: value]
			}
		}

		return Maybe[none]
	}

	on [findIndex: func (Func[Bool, T, Int])] (Int) {
		for my i, my value in: this {
			if func[call: value, i] {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeFindIndex: func (Func[Bool, T, Int])] (Maybe[Int]) {
		for my i, my value in: this {
			if func[call: value, i] {
				return Maybe[the: i]
			}
		}

		return Maybe[none]
	}
	
	
	;== Rotating

	on [rotate: offset (Int)] (This) {
		if offset < 0 {
			return this[rotateLeft: -offset]
		} else {
			return this[rotateRight: offset]
		}
	}

	on [rotateLeft: offset (Int)] (This) {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset ?= 0 || this.length < 2 => return this[new]
			else => return this[upto: offset] + this[from: offset]
		}
	}

	on [rotateRight: offset (Int)] (This) {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset ?= 0 || this.length < 2 => return this[new]
			else {
				my offset' = length - offset
				return this[upto: offset'] + this[from: offset']
			}
		}
	}
	
	
	;== Comparing
	
	operator `?=` [other (This)] (Bool) {
		return this.length ?= other.length && {
			for my i from: 0 upto: this.length {
				if this[Unsafe at: i] != other[Unsafe at: i] {
					return false
				}
			}
			
			return true
		}
	}
	
	operator `!=` [other (This)] (Bool) {
		return this.length != other.length || {
			for my i from: 0 upto: this.length {
				if this[Unsafe at: i] != other[Unsafe at: i] {
					return true
				}
			}
			
			return false
		}
	}
}

type T of Comparable
protocol Positional[T] of Comparable {
	;== Comparing
	
	operator `>` [other (This)] (Bool) {
		match this.length {
			at _ > other.length => return true
			at _ < other.length => return false
			at 0 => return false
			else {
				for my i from: 0 upto: this.length {
					if this[Unsafe at: i] <= other[Unsafe at: i] {
						return false
					}
				}
				
				return true
			}
		}
	}
	
	operator `>=` [other (This)] (Bool) {
		match this.length {
			at _ > other.length => return true
			at _ < other.length => return false
			at 0 => return true
			else {
				for my i from: 0 upto: this.length {
					if this[Unsafe at: i] < other[Unsafe at: i] {
						return false
					}
				}
				
				return true
			}
		}
	}
	
	operator `<` [other (This)] (Bool) {
		match this.length {
			at _ < other.length => return true
			at _ > other.length => return false
			at 0 => return false
			else {
				for my i from: 0 upto: this.length {
					if this[Unsafe at: i] >= other[Unsafe at: i] {
						return false
					}
				}
				
				return true
			}
		}
	}
	
	operator `<=` [other (This)] (Bool) {
		match this.length {
			at _ < other.length => return true
			at _ > other.length => return false
			at 0 => return true
			else {
				for my i from: 0 upto: this.length {
					if this[Unsafe at: i] > other[Unsafe at: i] {
						return false
					}
				}
				
				return true
			}
		}
	}
	
	
	;== Querying
	
	on [min] (T) {
		if this.length ?= 0 {
			throw NotFound[new]
		} else {
			my min = this[Unsafe at: 0]
			
			for my i from: 1 upto: this.length {
				my value = this[Unsafe at: i]
				
				if value < min {
					min = value
				}
			}
			
			return min
		}
	}
	
	;on [min: count (Int)] (This)
	
	;on [allMin] (This)
	
	on [max] (T) {
		if this.length ?= 0 {
			throw NotFound[new]
		} else {
			my max = this[Unsafe at: 0]
			
			for my i from: 1 upto: this.length {
				my value = this[Unsafe at: i]
				
				if value > max {
					max = value
				}
			}
			
			return max
		}
	}
	
	;on [max: count (Int)] (This)
	
	;on [allMax] (This)
}

;[
TODO:
- more insertion/deletion stuff
]