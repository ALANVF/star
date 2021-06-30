;@@ TODO: figure out how to include Array#every:?by:?allowPartial: as a generic method

type T
protocol Positional[T] of Iterable[T] {
	;== Creating
	
	init [new: capacity (Int)] {
		this[InPlace new]
	}
	
	
	;== Sizing
	
	on [length] (Int) is getter
	
	
	;== Accessing
	
	on [at: index (Int)] (T)
	on [at: index (Int) set: value (T)] is setter
	
	on [maybeAt: index (Int)] (Maybe[T])
	on [maybeAt: index (Int) set: value (T)] is setter
	
	
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
			this[:from set: values]
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
			this[to: upto - 1 set: values]
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
	
	
	;== Removing elements

	on [removeAt: index (Int)] (T)
	on [maybeRemoveAt: index (Int)] (Maybe[T])
	
	
	;== Removing sections

	on [removeFrom: from (Int)] (This)
	
	on [removeTo: to (Int)] (This)
	
	on [removeFrom: from (Int) to: (Int)] (This)
	
	on [removeAfter: after (Int)] (This) {
		return this[removeFrom: after + 1]
	}
	
	; ... and all other variants of from/after/to/upto
	
	
	;== Clearing
	
	on [clear]
	
	
	;== Appending
	
	on [add: value (T)] (T)
	
	type Iter of Iterable[T]
	on [addAll: values (Iter)] (Iter) {
		for my value in: values {
			this[add: value]
		}
		
		return values
	}
	
	
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
	
	
	;== Iterating
	
	on [Iterator[T]]
	
	on [each: func (Func[Void, T, Int])] {
		for my i from: 0 upto: this.length {
			func[call: this[Unsafe at: i], i]
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
		
		for my i from: 0 upto: this.length {
			my value = this[Unsafe at: i]
			
			if func[call: value, i] {
				result[add: value]
			}
		}
		
		return result
	}
	
	on [keepWhile: func (Func[Bool, T, Int])] (This) {
		my result = This[new: this.length // 2]
		
		for my i from: 0 upto: this.length {
			my value = this[Unsafe at: i]
			
			if func[call: value, i] {
				result[add: value]
			} else {
				break
			}
		}
		
		return result
	}
	
	
	;== Observing

	on [all: func (Func[Bool, T, Int])] (Bool) {
		for my i from: 0 upto: this.length {
			if !func[call: this[Unsafe at: i], i] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, T, Int])] (Bool) {
		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, T, Int])] (Bool) {
		my cond = false

		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
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
		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
				return false
			}
		}

		return true
	}

	on [contains: value (T)] (Bool) {
		for my i from: 0 upto: this.length {
			if this[Unsafe at: i] ?= value {
				return true
			}
		}

		return false
	}

	on [containsIndex: index (Int)] (Bool) {
		return 0 <= index < this.length
	}
	
	
	;== Counting

	on [count: value (T)] (Int) {
		my count = 0

		for my i from: 0 upto: this.length {
			if this[Unsafe at: i] ?= value {
				count++
			}
		}

		return count
	}

	on [countIf: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
				count++
			}
		}

		return count
	}

	on [countWhile: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
				count++
			} else {
				break
			}
		}

		return count
	}
	
	
	;== Finding

	on [indexOf: value (T)] (Int) {
		for my i from: 0 upto: this.length {
			if this[Unsafe at: i] ?= value {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeIndexOf: value (T)] (Int) {
		for my i from: 0 upto: this.length {
			if this[Unsafe at: i] ?= value {
				return Maybe[the: i]
			}
		}

		return Maybe[none]
	}

	on [find: func (Func[Bool, T, Int])] (T) {
		for my i from: 0 upto: this.length {
			my value = this[Unsafe at: i]

			if func[call: value, i] {
				return value
			}
		}

		throw NotFound[new]
	}

	on [maybeFind: func (Func[Bool, T, Int])] (Maybe[T]) {
		for my i from: 0 upto: this.length {
			my value = this[Unsafe at: i]

			if func[call: value, i] {
				return Maybe[the: value]
			}
		}

		return Maybe[none]
	}

	on [findIndex: func (Func[Bool, T, Int])] (Int) {
		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeFindIndex: func (Func[Bool, T, Int])] (Maybe[Int]) {
		for my i from: 0 upto: this.length {
			if func[call: this[Unsafe at: i], i] {
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
			at offset ?= 1 => return this[new]
			else => return this[upto: offset] + this[from: offset]
		}
	}

	on [rotateRight: offset (Int)] (This) {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset ?= 1 => return this[new]
			else {
				my offset' = length - offset
				return this[upto: offset'] + this[from: offset']
			}
		}
	}
	
	
	;== Checking
	
	operator `?` (Bool) {
		return this.length != 0
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
	
	
	;== Concating
	
	operator `+` [other (This)] (This) {
		return This[new: this.length + other.length]
		-> [addAll: this]
		-> [addAll: other]
	}
	
	
	;== Repeating
	
	operator `*` [count (Int)] (This) {
		if count < 0 {
			throw "invalid count"
		}
		
		my result = This[new: this.length * count]
		
		for _ from: 1 to: count {
			result[addAll: this]
		}
		
		return result
	}
}

type T of Comparable
protocol Positional[T] {
	;== Comparing
	
	operator `>` [other (This)] (Bool) {
		case {
			at this.length > other.length => return true
			at this.length < other.length => return false
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
		case {
			at this.length > other.length => return true
			at this.length < other.length => return false
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
		case {
			at this.length < other.length => return true
			at this.length > other.length => return false
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
		case {
			at this.length < other.length => return true
			at this.length > other.length => return false
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
	
	on [maybeMin] (Maybe[T]) {
		try {
			return Maybe[the: this[Inline min]]
		} catch {
			at NotFound => return Maybe[none]
		}
	}
	
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
	
	on [maybeMax] (Maybe[T]) {
		try {
			return Maybe[the: this[Inline max]]
		} catch {
			at NotFound => return Maybe[none]
		}
	}
	
	;on [allMax] (This)
}

;[
TODO:
- more insertion/deletion stuff
]