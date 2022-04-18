type T
protocol Collection[T] of Iterable[T] {
	;== Creating
	
	init [new: capacity (Int)] => #init_this This[new]


	;== Sizing
	
	on [length] (Int) is getter


	;== Adding values

	on [add: value (T)] (T)

	on [maybeAdd: value (Maybe[T])] (Maybe[T]) {
		match value at Maybe[the: my value'] {
			this[add: value']
		}

		return value
	}

	type Iter of Iterable[T]
	on [addAll: values (Iter)] (Iter) {
		for my value in: values {
			this[add: value]
		}
		
		return values
	}
	
	
	;== Removing values
	
	on [remove: value (T)] (Bool)
	on [remove: value (T) times: (Int)] (Int)
	on [removeAll: value (T)] (Int)
	
	on [removeValues: values (Iterable[T])] (Bool)
	on [removeValues: (Iterable[T]) times: (Int)] (This) => return this[This :removeValues :times] [Unsafe This] ;@@ TODO: fix
	on [removeAllValues: (Iterable[T])] (This) => return this[This :removeAllValues] [Unsafe This] ;@@ TODO: fix
	
	on [removeWhere: func (Func[Bool, T])] (Bool)
	on [removeWhere: (Func[Bool, T]) times: (Int)] (This) => return this[This :removeWhere :times] [Unsafe This] ;@@ TODO: fix
	on [removeAllWhere: (Func[Bool, T])] (This) => return this[This :removeAllWhere] [Unsafe This] ;@@ TODO: fix


	;== Clearing
	
	on [clear]


	;== Iterating
	
	on [each: func (Func[Void, T])] is inline {
		for my value in: this {
			func[call: value]
		}
	}


	;== Filtering

	on [keepIf: func (Func[Bool, T])] (This) {
		my result = This[new: this.length // 2]
		
		for my value in: this {
			if func[call: value] {
				result[add: value]
			}
		}
		
		return result
	}
	
	on [keepWhile: func (Func[Bool, T])] (This) {
		my result = This[new: this.length // 2]
		
		for my value in: this while: func[call: value] {
			result[add: value]
		}
		
		return result
	}
	
	
	;== Observing

	on [all: func (Func[Bool, T])] (Bool) {
		for my value in: this {
			if !func[call: value] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, T])] (Bool) {
		for my value in: this {
			if func[call: value] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, T])] (Bool) {
		my cond = false

		for my value in: this {
			if func[call: value] {
				if cond {
					return false
				} else {
					cond = true
				}
			}
		}

		return cond
	}

	on [none: func (Func[Bool, T])] (Bool) {
		for my value in: this {
			if func[call: value] {
				return false
			}
		}

		return true
	}


	;== Checking

	on [contains: value (T)] (Bool) {
		for my value' in: this {
			if value ?= value' {
				return true
			}
		}

		return false
	}

	;@@ TODO: figure out how to remove extra allocations
	on [containsAll: values (Iterable[T])] (Bool) {
		my elems = this[new]
		for my value in: values {
			if !elems[remove: value] {
				return false
			}
		}

		return true
	}

	on [containsAny: values (Iterable[T])] (Bool) {
		for my value in: this {
			for my value' in: values {
				if value ?= value' {
					return true
				}
			}
		}

		return false
	}

	on [containsOne: values (Iterable[T])] (Bool) {
		my found = Maybe[none]
		for my value in: values {
			for my value' in: this {
				if value ?= value' {
					match found {
						at Maybe[the: value'] => break
						at Maybe[the: _] => return false
						at Maybe[none] {
							found = Maybe[the: value']
							break
						}
					}
				}
			}
		}

		return found?
	}

	on [containsNone: values (Iterable[T])] (Bool) {
		for my value in: this {
			for my value' in: values {
				if value ?= value' {
					return false
				}
			}
		}

		return true
	}
	
	operator `?` (Bool) is inline => return this.length != 0

	operator `!` (Bool) is inline => return this.length ?= 0


	;== Counting

	on [count: value (T)] (Int) {
		my count = 0

		for my value' in: this {
			if value' ?= value {
				count++
			}
		}

		return count
	}

	on [countIf: func (Func[Bool, T])] (Int) {
		my count = 0

		for my value in: this {
			if func[call: value] {
				count++
			}
		}

		return count
	}

	on [countWhile: func (Func[Bool, T])] (Int) {
		my count = 0

		for my value in: this while: func[call: value] {
			count++
		}

		return count
	}


	;== Finding

	on [find: func (Func[Bool, T])] (T) {
		for my value in: this {
			if func[call: value] {
				return value
			}
		}

		throw NotFound[new]
	}

	on [maybeFind: func (Func[Bool, T])] (Maybe[T]) {
		for my value in: this {
			if func[call: value] {
				return Maybe[the: value]
			}
		}

		return Maybe[none]
	}


	;== Concating
	
	operator `+` [other (This)] (This) {
		return This[new: this.length + other.length]
		-> [addAll: this]
		-> [addAll: other]
	}


	;== Repeating
	
	operator `*` [count (Int)] (This) {
		if count < 0 => throw "invalid count"
		
		my result = This[new: this.length * count]
		
		for _ from: 1 times: count {
			result[addAll: this]
		}
		
		return result
	}
}

type T of Comparable
protocol Collection[T] of Comparable {
	;== Comparing
	
	operator `>` [other (This)] (Bool)
	operator `<` [other (This)] (Bool)


	;== Querying
	
	on [min] (T) {
		if this.length ?= 0 {
			throw NotFound[new]
		} else {
			my min (T)
			
			for my i, my value in: this {
				if i ?= 0 || value < min {
					min = value
				}
			}
			
			return min ; unsafe?
		}
	}
	
	on [maybeMin] (Maybe[T]) {
		try {
			return Maybe[the: #inline this[min]]
		} catch {
			at NotFound => return Maybe[none]
		}
	}

	on [max] (T) {
		if this.length ?= 0 {
			throw NotFound[new]
		} else {
			my max (T)
			
			for my i, my value in: this {
				if i ?= 0 || value > max {
					max = value
				}
			}
			
			return max ; unsafe?
		}
	}
	
	on [maybeMax] (Maybe[T]) {
		try {
			return Maybe[the: #inline this[max]]
		} catch {
			at NotFound => return Maybe[none]
		}
	}
}


type T
type U
type C of Collection[U]
category C for Collection[T] {
	;== Removing values
	
	on [removeValues: values (Iterable[T])] (C)
	on [removeValues: values (Iterable[T]) times: (Int)] (C)
	on [removeAllValues: values (Iterable[T])] (C)
	
	on [removeWhere: func (Func[Bool, T]) times: (Int)] (C)
	on [removeAllWhere: func (Func[Bool, T])] (C)


	;== Collecting

	on [collect: func (Func[U, T])] (C) {
		my result = C[new]
		
		for my value in: this {
			result[add: func[call: value]]
		}

		return result
	}
}