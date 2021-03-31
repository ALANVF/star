use Native

type T
protocol Values[T] of Iterable[T] {
	my buffer (Ptr[T])
	my length (Int)
	my capacity (Int) = length


	;== Creating

	init [new] {
		buffer = Ptr[new]
		length = capacity = 0
	}

	init [new: capacity (Int)] {
		buffer = Ptr[new: capacity]
		length = 0
		this.capacity = capacity
	}


	;== Sizing

	on [resizeTo: newCapacity (Int)] is hidden {
		if newCapacity < 0 {
			throw LengthError[new: newCapacity]
		}

		if newCapacity > capacity {
			buffer = buffer[resized: newCapacity]
			capacity = newCapacity
		} orif newCapacity < length {
			length = newCapacity
		}
	}

	on [resizeBy: extraCapacity (Int)] is hidden is inline {
		this[resizeTo: capacity + extraCapacity]
	}


	;== Moving

	on [from: (Int) moveInto: dest (Ptr[T])] is hidden is inline {
		[buffer + from move: length - from to: dest]
	}

	on [from: (Int) moveTo: index (Int)] is hidden is inline {
		this[:from moveInto: buffer + index]
	}

	on [from: (Int) moveBy: offset (Int)] is hidden is inline {
		this[:from moveTo: from + offset]
	}

	on [after: (Int) moveBy: offset (Int)] is hidden is inline {
		this[from: after + 1 moveBy: offset]
	}


	;== Copying

	on [new] (This) {
		return This[buffer: buffer[new: length] :length]
	}

	on [copyInto: dest (Ptr[T])] is hidden {
		buffer[copy: length to: dest]
	}
	
	on [from: (Int) copyInto: dest (Ptr[T])] is hidden {
		[buffer + from copy: length - from to: dest]
	}

	on [from: (Int) copyTo: index (Int)] is hidden is inline {
		this[:from copyInto: buffer + index]
	}

	on [from: (Int) copyBy: offset (Int)] is hidden is inline {
		this[:from copyTo: from + offset]
	}

	;[on [to: (Int) copyInto: dest (Ptr[T])] is hidden {
		for my i from: 0 to: to {
			this[Unsafe at: i] = values[at: i]
		}
	}]


	;== Accessing
	
	on [at: index (Int)] (T) {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			return buffer[at: index]
		} else {
			throw IndexError[at: index]
		}
	}

	on [at: index (Int) set: value (T)] is setter {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			buffer[at: index] = value
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeAt: index (Int)] (T) {
		if index < 0 {
			index += length
		}
		
		if 0 <= index < length {
			return Maybe[the: buffer[at: index]]
		} else {
			return Maybe[none]
		}
	}

	on [maybeAt: index (Int) set: value (T)] is setter {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			buffer[at: index] = value
		}
	}


	;== Slicing

	on [from: (Int)] (This) {
		if from < 0 {
			from += length
		}

		if 0 <= from < length {
			my newLength = length - from

			return This[
				buffer: [buffer + from copy: newLength]
				length: newLength
			]
		} else {
			throw RangeError[:from to: length - 1]
		}
	}

	on [from: (Int) set: values (This)] is setter {
		if from < 0 {
			from += length
		}

		if 0 <= from < length {
			my maxAfterIndex = index + values.length
			my diff = maxAfterIndex - length

			if diff > 0 {
				this[resizeBy: diff]
			}

			values[copyInto: buffer + index]
		} else {
			throw RangeError[:from to: length - 1]
		}
	}

	on [to: (Int)] (This) {
		if to < 0 {
			to += length
		}

		if 0 <= to < length {
			return This[
				buffer: buffer[copy: to]
				length: to
			]
		} else {
			throw RangeError[from: 0 :to]
		}
	}

	on [to: (Int) set: values (This)] is setter {
		if to < 0 {
			to += length
		}

		if 0 <= to < length {
			my diff = values.length - to

			if diff < 0 {
				;-- Move and then resize
				this[from: to moveBy: diff]
				this[resizeBy: diff[abs]]
			} orif diff > 0 {
				;-- Resize and then move
				this[resizeBy: diff]
				this[from: to moveBy: diff]
			}

			values[copyInto: buffer]
		} else {
			throw RangeError[from: 0 :to]
		}
	}

	on [from: (Int) to: (Int)] (This) {
		if from < 0 {
			from += length
		}

		if to < 0 {
			to += length
		}

		if 0 <= from <= to < length {
			my newLength = to - from

			return This[
				buffer: [buffer + from copy: newLength]
				length: newLength
			]
		} else {
			throw RangeError[:from :to]
		}
	}

	;on [from: (Int) to: (Int) set: values (This)] is setter

	on [after: (Int)] (This) {
		my from = after + 1

		if from ?= length {
			return This[new]
		} else {
			return this[:from]
		}
	}

	on [after: (Int) set: values (This)] is setter {
		my from = after + 1
		
		if from ?= length {
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

	on [removeAt: index (Int)] (T) {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			my value = buffer[at: index]

			this[after: index moveBy: -1]
			
			return value
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeRemoveAt: index (Int)] (Maybe[T]) {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			my value = buffer[at: index]

			this[after: index moveBy: -1]
			
			return Maybe[the: value]
		} else {
			return Maybe[none]
		}
	}


	;== Removing sections

	on [removeFrom: from (Int)] (This) {
		if from < 0 {
			from += length
		}

		if 0 <= from < length {
			this[resizeTo: from]
		} else {
			throw RangeError[:from]
		}
	}

	on [removeAfter: after (Int)] (This) is inline {
		return this[removeFrom: after + 1]
	}

	; ... and all other variants of from/after/to/upto
	
	
	;== Clearing
	
	on [clear] {
		length = 0
	}


	;== Appending

	on [add: value (T)] (T) {
		this[resizeBy: 1]
		ptr[at: length++] = value

		return value
	}

	on [addAll: values (This)] (This) {
		this[resizeBy: values.length]
		values[copyInto: buffer + length]
		length += values.length

		return values
	}

	type Iter of Iterable[T]
	on [addAll: values (Iter)] (Iter) {
		for my value in: values {
			this[add: value]
		}

		return values
	}


	;== Prepending

	on [prepend: value (T)] (T) {
		this[resizeBy: 1]
		this[from: 0 moveBy: 1]
		ptr[at: 0] = value
		length++

		return value
	}

	on [prependAll: values (This)] (This) {
		this[resizeBy: values.length]
		this[from: 0 moveBy: length]
		values[copyInto: buffer]
		length += values.length

		return values
	}

	type Iter of Iterable[T]
	on [prependAll: values (Iter)] (Iter) {
		this[prependAll: This[new] -> {
			for my value in: values {
				this[add: value]
			}
		}]

		return values
	}


	;== Iterating

	on [Iterator[T]] is inline {
		return ValuesIterator[new: this]
	}

	on [each: func (Func[Void, T, Int])] {
		for my i from: 0 upto: length {
			func[call: buffer[at: i], i]
		}
	}


	;== Reversing

	on [reverse] (This) {
		match length {
			at 0 || 1 => return this[new]
			
			at 2 {
				return This[new: 2]
				-> [add: buffer[at: 1]]
				-> [add: buffer[at: 0]]
			}

			else => return This[new: length] -> {
				for my i after: _.length downto: 0 {
					result[add: _.buffer[at: i]]
				}
			}
		}
	}


	;== Filtering

	on [keepIf: func (Func[Bool, T, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				my value = _.buffer[at: i]
				
				if func[call: value, i] {
					this[add: value]
				}
			}
		}
	}

	on [keepWhile: func (Func[Bool, T, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				my value = _.buffer[at: i]
				
				if func[call: value, i] {
					this[add: value]
				} else {
					break
				}
			}
		}
	}


	;== Observing

	on [all: func (Func[Bool, T, Int])] (Bool) {
		for my i from: 0 upto: length {
			if !func[call: buffer[at: i], i] {
				return false
			}
		}

		return true
	}

	on [any: func (Func[Bool, T, Int])] (Bool) {
		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
				return true
			}
		}

		return false
	}

	on [one: func (Func[Bool, T, Int])] (Bool) {
		my cond = false

		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
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
		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
				return false
			}
		}

		return true
	}

	on [contains: value (T)] (Bool) {
		for my i from: 0 upto: length {
			if buffer[at: i] ?= value {
				return true
			}
		}

		return false
	}

	on [containsIndex: index (Int)] (Bool) {
		return 0 <= index < length
	}


	;== Counting

	on [count: value (T)] (Int) {
		my count = 0

		for my i from: 0 upto: length {
			if buffer[at: i] ?= value {
				count++
			}
		}

		return count
	}

	on [countIf: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
				count++
			}
		}

		return count
	}

	on [countWhile: func (Func[Bool, T, Int])] (Int) {
		my count = 0

		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
				count++
			} else {
				break
			}
		}

		return count
	}


	;== Finding

	on [indexOf: value (T)] (Int) {
		for my i from: 0 upto: length {
			if buffer[at: i] ?= value {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeIndexOf: value (T)] (Int) {
		for my i from: 0 upto: length {
			if buffer[at: i] ?= value {
				return Maybe[the: i]
			}
		}

		return Maybe[none]
	}

	on [find: func (Func[Bool, T, Int])] (T) {
		for my i from: 0 upto: length {
			my value = buffer[at: i]

			if func[call: value, i] {
				return value
			}
		}

		throw NotFound[new]
	}

	on [maybeFind: func (Func[Bool, T, Int])] (Maybe[T]) {
		for my i from: 0 upto: length {
			my value = buffer[at: i]

			if func[call: value, i] {
				return Maybe[the: value]
			}
		}

		return Maybe[none]
	}

	on [findIndex: func (Func[Bool, T, Int])] (Int) {
		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
				return i
			}
		}

		throw NotFound[new]
	}

	on [maybeFindIndex: func (Func[Bool, T, Int])] (Maybe[Int]) {
		for my i from: 0 upto: length {
			if func[call: buffer[at: i], i] {
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
			at offset < 2 => return this[new]
			else => return this[upto: offset] + this[from: offset]
		}
	}

	on [rotateRight: offset (Int)] (This) {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset < 2 => return this[new]
			else {
				my offset' = length - offset
				return this[upto: offset'] + this[from: offset']
			}
		}
	}
	
	
	;== Checking
	
	operator `?` (Bool) is inline {
		return length != 0
	}
	
	
	;== Comparing
	
	operator `?=` [other (This)] (Bool) {
		return length ?= other.length && {
			for my i from: 0 upto: length {
				if buffer[at: i] != other[Unsafe at: i] {
					return false
				}
			}
			
			return true
		}
	}
	
	operator `!=` [other (This)] (Bool) {
		return length != other.length || {
			for my i from: 0 upto: length {
				if buffer[at: i] != other[Unsafe at: i] {
					return true
				}
			}
			
			return false
		}
	}
}

;[
TODO:
- more insertion/deletion stuff
]