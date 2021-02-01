type T
class Series[T] of Ordered, Iterable[T] {
	my buffer (Values[T]) is hidden
	my offset (Int) is getter


	;== Creation

	init [new: values (Values[T])] {
		buffer = values[new]
		offset = 0
	}

	
	;== Length

	on [length] (Int) is getter is inline {
		return buffer.length - offset
	}


	;== Copying

	on [new] (This) {
		return This[buffer: buffer[from: offset] offset: 0]
	}


	;== Accessing

	on [first] (T) is inline {
		return this[at: 0]
	}

	on [last] (T) is inline {
		return this[at: this.length - 1]
	}

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

	on [isHead] (Bool) is inline {
		return offset ?= 0
	}


	;== Tail

	on [tail] (This) {
		if this[isTail] {
			return this
		} else {
			return This[:buffer offset: buffer.length]
		}
	}

	on [isTail] (Bool) is inline {
		return offset ?= buffer.length
	}


	;== Skipping

	on [skip: by (Int)] (This) {
		my offset' = offset + by

		if 0 <= offset' <= buffer.length {
			return This[:buffer offset: offset']
		} else {
			throw "Out of bounds!"
		}
	}

	on [next] (This) is inline {
		return this[skip: 1]
	}

	
	on [previous] (This) is inline {
		return this[skip: -1]
	}


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


	;== Iterating

	on [Iterator[T]] is inline {
		return ValuesIterator[newWithoutCopying: buffer[from: offset]]
	}
}