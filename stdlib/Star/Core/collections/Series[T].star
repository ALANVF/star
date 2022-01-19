type T
class Series[T] of Ordered, Iterable[T] {
	my buffer (Values[T]) is hidden
	my offset (Int) is getter


	;== Creating (macros)

	init [_: values (Array[T])] is macro {
		buffer = #expand Array[T][_: values]
		offset = 0
	}
	
	
	;== Creating
	type V of Values[T]
	init [new: values (V)] {
		buffer = values[new]
		offset = 0
	}

	
	;== Length

	on [length] (Int) is getter is inline => return buffer.length - offset


	;== Copying

	on [new] (This) => return This[buffer: buffer[from: offset] offset: 0]


	;== Accessing

	on [first] (T) is inline => return this[at: 0]

	on [last] (T) is inline => return this[at: this.length - 1]

	on [at: index (Int)] (T) {
		my index' = index + offset
		
		if 0 <= index' < buffer.length {
			return buffer[Unsafe at: index']
		} else {
			throw IndexError[at: index]
		}
	}

	on [at: index (Int) set: value (T)] is setter {
		my index' = index + offset
		
		if 0 <= index' < buffer.length {
			buffer[Unsafe at: index'] = value
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeAt: index (Int)] (Maybe[T]) {
		my index' = index + offset
		
		if 0 <= index' < buffer.length {
			return Maybe[the: buffer[Unsafe at: index']]
		} else {
			return Maybe[none]
		}
	}

	on [maybeAt: index (Int) set: value (T)] is setter {
		my index' = index + offset
		
		if 0 <= index' < buffer.length {
			buffer[Unsafe at: index'] = value
		}
	}


	;== Head

	on [head] (This) {
		if this[isHead] {
			return this
		} else {
			return This[:buffer offset: 0]
		}
	}

	on [isHead] (Bool) is inline => return offset ?= 0


	;== Tail

	on [tail] (This) {
		if this[isTail] {
			return this
		} else {
			return This[:buffer offset: buffer.length]
		}
	}

	on [isTail] (Bool) is inline => return offset ?= buffer.length


	;== Skipping

	on [skip: by (Int)] (This) {
		my offset' = offset + by

		if 0 <= offset' <= buffer.length {
			return This[:buffer offset: offset']
		} else {
			throw IndexError[at: offset']
		}
	}

	on [next] (This) is inline => return this[skip: 1]
	
	on [previous] (This) is inline => return this[skip: -1]


	;== Comparing

	on [sameSeries: other (This)] (Bool) is hidden is inline {
		return buffer[Native address] ?= other.buffer[Native address]
	}

	operator `?=` [other (This)] (Bool) {
		return this[sameSeries: other] && offset ?= other.offset
	}

	operator `>` [other (This)] (Bool) {
		return this[sameSeries: other] && offset > other.offset
	}

	operator `<` [other (This)] (Bool) {
		return this[sameSeries: other] && offset < other.offset
	}


	;== Checking

	operator `?` (Bool) => return buffer.length - offset > 0


	;== Iterating

	on [Iterator[T]] is inline {
		return ValuesIterator[Unsafe newWithoutCopying: buffer[from: offset]]
	}


	;== Finding

	on [maybeFind: value (T)] (Maybe[This]) {
		if this[isTail] {
			return Maybe[none]
		} else {
			for my i from: 0 upto: this.length {
				if this[at: i] ?= value {
					return Maybe[the: this[skip: i]]
				}
			}

			return Maybe[none]
		}
	}
	
	on [maybeFindEach: values (Positional[T])] (Maybe[This]) {
		if this[isTail] || values.length > this.length {
			return Maybe[none]
		} else {
			for my i from: 0 to: this.length - values.length label: `outer` {
				for my j from: 0 to: values.length {
					if this[at: i + j] != values[at: j] {
						next `outer`
					}
				}

				return Maybe[the: this[skip: i]]
			}

			return Maybe[none]
		}
	}


	;== Observing

	on [startsWith: value (T)] (Bool) {
		if this[isTail] {
			return false
		} else {
			return this[first] ?= value
		}
	}

	type V of Positional[T]
	on [startsWithEach: values (V)] (Bool) {
		if this[isTail] || values.length > this.length {
			return false
		} else {
			for my i from: 0 upto: values.length {
				if this[at: i] != values[at: i] {
					return false
				}
			}

			return true
		}
	}


	;== Adding

	on [add: value (T)] (T) is inline => return buffer[add: value]
	
	
	;== Inserting
	
	on [insert: value (T)] (T) is inline => return buffer[at: offset add: value]
	
	
	;== Removing
	
	on [remove] (T) is inline => return buffer[removeAt: offset]
	
	on [removeAt: index (Int)] (T) is inline => return buffer[removeAt: index + offset]
}