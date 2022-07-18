use Native

type T
protocol Values[T] of Positional[T] {
	my buffer (Ptr[T]) is hidden
	my length (Int) is getter
	my capacity (Int) is getter = length


	;== Creating

	init [new] {
		buffer = Ptr[new]
		length = capacity = 0
	}

	init [new: capacity (Int)] {
		buffer = Ptr[new: capacity]
		length = 0
		_.capacity = capacity
	}

	init [fill: size (Int) with: value (T)] {
		buffer = Ptr[new: size]
		length = capacity = size
		for my i from: 0 times: size {
			buffer[at: i] = value
		}
	}


	;== Sizing

	on [resizeTo: newCapacity (Int)] is hidden {
		if newCapacity < 0 => throw LengthError[new: newCapacity]

		case {
			at newCapacity > capacity {
				buffer = buffer[resized: newCapacity]
				capacity = newCapacity
			}
			at newCapacity < length {
				length = newCapacity
			}
		}
	}

	on [resizeBy: extraCapacity (Int)] is hidden is inline => this[resizeTo: capacity + extraCapacity]


	;== Moving

	on [from: (Int) moveInto: dest (Ptr[T])] is hidden is inline => [buffer + from move: length - from to: dest]

	on [from: (Int) moveTo: index (Int)] is hidden is inline => this[:from moveInto: buffer + index]

	on [from: (Int) moveBy: offset (Int)] is hidden is inline => this[:from moveTo: from + offset]

	on [after: (Int) moveBy: offset (Int)] is hidden is inline => this[from: after + 1 moveBy: offset]


	;== Copying

	on [new] (This) => return This[buffer: buffer[copy: length] :length]

	on [copyInto: dest (Ptr[T])] is hidden => buffer[copy: length to: dest]
	
	on [from: (Int) copyInto: dest (Ptr[T])] is hidden => [buffer + from copy: length - from to: dest]

	on [from: (Int) copyTo: index (Int)] is hidden is inline => this[:from copyInto: buffer + index]

	on [from: (Int) copyBy: offset (Int)] is hidden is inline => this[:from copyTo: from + offset]

	;[on [to: (Int) copyInto: dest (Ptr[T])] is hidden {
		for my i from: 0 to: to {
			this[Unsafe at: i] = values[at: i]
		}
	}]


	;== Accessing
	
	on [at: index (Int)] (T) {
		if index < 0 => index += length

		if 0 <= index < length {
			return buffer[at: index]
		} else {
			throw IndexError[at: index]
		}
	}

	on [at: index (Int) set: value (T)] is setter {
		if index < 0 => index += length

		if 0 <= index < length {
			buffer[at: index] = value
		} else {
			throw IndexError[at: index]
		}
	}


	;== Slicing

	on [from: (Int)] (This) {
		if from < 0 => from += length

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
		if from < 0 => from += length

		if 0 <= from < length {
			my maxAfterIndex = from + values.length
			my diff = maxAfterIndex - length

			if diff > 0 {
				this[resizeBy: diff]
			}

			values[copyInto: buffer + from]
		} else {
			throw RangeError[:from to: length - 1]
		}
	}

	on [to: (Int)] (This) {
		if to < 0 => to += length

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
		if to < 0 => to += length

		if 0 <= to < length {
			my diff = values.length - to

			case {
				at diff < 0 {
					;-- Move and then resize
					this[from: to moveBy: diff]
					this[resizeBy: diff[abs]]
				}
				at diff > 0 {
					;-- Resize and then move
					this[resizeBy: diff]
					this[from: to moveBy: diff]
				}
			}

			values[copyInto: buffer]
		} else {
			throw RangeError[from: 0 :to]
		}
	}

	on [from: (Int) to: (Int)] (This) {
		if from < 0 => from += length
		if to < 0 => to += length

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


	;== Inserting values

	on [at: index (Int) add: value (T)] (T) {
		if index < 0 => index += length

		if 0 <= index < length {
			this[resizeBy: 1]
			this[from: index moveBy: 1]
			buffer[at: index] = value
			length++

			return value
		} else {
			throw IndexError[at: index]
		}
	}

	type V of Values[T]
	on [at: index (Int) addAll: values (V)] (V) {
		if index < 0 => index += length

		if 0 <= index < length {
			my valuesLen = values.length
			this[resizeBy: valuesLen]
			this[from: index moveBy: valuesLen]
			values[copyInto: buffer + index]
			length += valuesLen

			return values
		} else {
			throw IndexError[at: index]
		}
	}
	
	type Pos of Positional[T]
	on [at: index (Int) addAll: values (Pos)] (Pos) {
		if index < 0 => index += length

		if 0 <= index < length {
			my valuesLen = values.length
			this[resizeBy: valuesLen]
			this[from: index moveBy: valuesLen]
			for my value in: values {
				buffer[at: index] = value
				index++
			}
			length += valuesLen

			return values
		} else {
			throw IndexError[at: index]
		}
	}

	type Iter of Iterable[T]
	on [at: index (Int) addAll: values (Iter)] (Iter) {
		if index < 0 => index += length

		if 0 <= index < length {
			for my value in: values {
				this[resizeBy: 1]
				this[from: index moveBy: 1]
				buffer[at: index] = value
				index++
				length++
			}

			return values
		} else {
			throw IndexError[at: index]
		}
	}


	;== Removing elements

	on [removeAt: index (Int)] (T) {
		if index < 0 => index += length

		if 0 <= index < length {
			my value = buffer[at: index]

			this[after: index moveBy: -1]
			
			return value
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeRemoveAt: index (Int)] (Maybe[T]) {
		if index < 0 => index += length

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
		if from < 0 => from += length

		if 0 <= from < length {
			my res = this[:from]

			this[resizeTo: from]
			
			return res
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
		buffer[at: length++] = value

		return value
	}

	type V of Values[T]
	on [addAll: values (V)] (V) {
		this[resizeBy: values.length]
		values[copyInto: buffer + length]
		length += values.length

		return values
	}


	;== Prepending

	on [prepend: value (T)] (T) {
		this[resizeBy: 1]
		this[from: 0 moveBy: 1]
		buffer[at: 0] = value
		length++

		return value
	}

	type V of Values[T]
	on [prependAll: values (V)] (V) {
		this[resizeBy: values.length]
		this[from: 0 moveBy: length]
		values[copyInto: buffer]
		length += values.length

		return values
	}


	;== Iterating

	on [Iterator[T]] is inline => return ValuesIterator[T][new: this]
}


type T
category Unsafe for Values[T] {
	;== Accessing
	
	on [at: index (Int)] (T) is inline {
		return buffer[at: index]
	}

	on [at: index (Int) set: value (T)] is setter is inline {
		buffer[at: index] = value
	}
	
	
	;== Removing elements
	
	on [removeAt: index (Int)] (T) {
		my value = buffer[at: index]

		this[after: index moveBy: -1]
		
		return value
	}
}