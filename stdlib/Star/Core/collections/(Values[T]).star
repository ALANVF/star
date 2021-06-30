use Native

type T
protocol Values[T] of Positional[T] {
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

	on [maybeAt: index (Int)] (Maybe[T]) {
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

	;on [removeTo: to (Int)] (This)
	;on [removeFrom: from (Int) to: (Int)] (This)
	
	
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


	;== Iterating

	on [Iterator[T]] is inline {
		return ValuesIterator[new: this]
	}
}