type T
category InPlace for Array[T] {
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
}