type T
category InPlace for Values[T] {
	;== Removing sections

	on [removeFrom: from (Int)] {
		if from < 0 => from += length

		if 0 <= from < length {
			this[resizeTo: from]
		} else {
			throw RangeError[:from]
		}
	}

	;on [removeTo: to (Int)]
	;on [removeFrom: from (Int) to: (Int)]


	;== Collecting

	on [collect: func (Func[T, T, Int])] {
		for my i from: 0 upto: length {
			buffer[at: i] = func[call: buffer[at: i], i]
		}
	}


	;== Filtering

	on [keepIf: func (Func[Bool, T])] {
		my i = 0
		while i < length {
			if !func[call: buffer[at: i]] {
				this[removeAt: i]
			}
		}
	}
	on [keepIf: func (Func[Bool, T, Int])] {
		my i = 0
		my i' = 0
		while i < length {
			if !func[call: buffer[at: i], i'++] {
				this[removeAt: i]
			}
		}
	}

	on [keepWhile: func (Func[Bool, T, Int])] {
		for my i from: 0 upto: length {
			if !func[call: buffer[at: i], i] {
				this[resizeTo: i]
				break
			}
		}
	}


	;== Collecting *and* filtering

	on [collectIf: func (Func[Maybe[T], T])] {
		my i = 0
		while i < length {
			match func[call: buffer[at: i]] at Maybe[the: my value] {
				buffer[at: i++] = value
			} else {
				this[removeAt: i]
			}
		}
	}
	on [collectIf: func (Func[Maybe[T], T, Int])] {
		my i = 0
		my i' = 0
		while i < length {
			match func[call: buffer[at: i], i'++] at Maybe[the: my value] {
				buffer[at: i++] = value
			} else {
				this[removeAt: i]
			}
		}
	}

	on [collectWhile: func (Func[Maybe[T], T, Int])] {
		for my i from: 0 upto: length {
			match func[call: buffer[at: i], i] at Maybe[the: my value] {
				buffer[at: i] = value
			} else {
				this[removeFrom: i]
				break
			}
		}
	}


	;== Reversing

	on [reverse] {
		match length {
			at 0 || 1 {}
			
			at 2 {
				my first = buffer[at: 0]

				buffer[at: 0] = buffer[at: 1]
				buffer[at: 1] = first
			}

			else {
				my middle = length // 2
				my left = middle - length % 2
				my right = middle + 1

				while left >= 0 {
					my leftValue = buffer[at: left]
					
					buffer[at: left--] = buffer[at: right]
					buffer[at: right++] = leftValue
				}
			}
		}
	}


	;== Rotating

	on [rotate: offset (Int)] {
		if offset < 0 {
			this[InPlace rotateLeft: -offset]
		} else {
			this[InPlace rotateRight: offset]
		}
	}

	on [rotateLeft: offset (Int)] {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset < 2 {}
			else {
				my offset' = length - offset
				my front = this[upto: offset]

				this[from: offset moveTo: 0]
				this[from: offset'] = front
			}
		}
	}

	on [rotateRight: offset (Int)] {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset < 2 {}
			else {
				my offset' = length - offset
				my end = this[from: offset']

				this[from: 0 moveBy: offset]
				this[upto: offset] = end
			}
		}
	}
}