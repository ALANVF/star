use Native

type T
class Array[T] of Values[T] {
	my buffer is hidden
	my length is getter
	my capacity is hidden


	;== Collecting

	type U
	on [collect: func (Func[U, T, Int])] (This[U]) {
		return This[U][new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: func[call: _.buffer[at: i], i]]
			}
		}
	}

	type U
	type V of Iterable[U]
	on [collectAll: func (Func[V, T, Int])] (This[U]) {
		return This[U][new: length] -> {
			for my i from: 0 upto: _.length {
				this[addAll: func[call: _.buffer[at: i], i]]
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


	;== Collecting *and* filtering

	type U
	on [collectIf: func (Func[Maybe[U], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				my value = _.buffer[at: i]
				
				if func[call: value, i] {
					this[add: value]
				}
			}
		}
	}

	on [collectWhile: func (Func[Bool, T, Int])] (This) {
		return This[U][new: length // 2] -> {
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

	type U
	type V of Iterable[U]
	on [collectAllIf: func (Func[Maybe[V], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				}
			}
		}
	}

	type U
	type V of Iterable[U]
	on [collectAllWhile: func (Func[Maybe[V], T, Int])] (This[U]) {
		return This[U][new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				} else {
					break
				}
			}
		}
	}


	;== Iterating

	on [each: func (Func[Void, T, Int])] {
		for my i from: 0 upto: length {
			func[call: buffer[at: i], i]
		}
	}
}