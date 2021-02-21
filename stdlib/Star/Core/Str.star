use Native

;[
==| TODO
]

class Str of Values[Char], Ordered is strong {
	my buffer is hidden
	my length is getter
	my capacity is hidden


	;== Creating

	init [new: char (Char)] {
		buffer = Ptr[new: 1]
		-> [at: 0] = char

		length = capacity = 1
	}

	type Chars of Values[Char]
	init [new: chars (Chars)] {
		my numChars = chars.length

		buffer = Ptr[new: numChars]
		length = capacity = numChars

		for my i from: 0 upto: numChars {
			buffer[at: i] = chars[at: i]
		}
	}

	init [new: chars (Iterable[Char])] {
		buffer = Ptr[new: 5]
		length = 0
		capacity = 5

		for my char in: chars {
			if length ?= capacity {
				capacity += 5
				buffer = buffer[resized: capacity]
			}

			buffer[at: length++] = char
		}
	}


	on [new] (This) {
		return This[buffer: buffer[copy: length] :length]
	}


	;== Chars

	on [chars] (Array[Char]) {
		my chars = Array[fill: length with: #"\0"]

		for my i from: 0 upto: length {
			chars[at: i] = buffer[at: i]
		}

		return chars
	}


	;== Hashing

	;[
	==| djb2 algorithm
	]
	on [hash] (Int) {
		my result = 5381

		for my i from: 0 upto: length {
			result = (result << 5) + result + buffer[at: i]
		}

		return result
	}


	;== Casing

	on [lowercase] (This) {
		return This[new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: _.buffer[at: i][lowercase]]
			}
		}
	}

	on [uppercase] (This) {
		return This[new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: _.buffer[at: i][uppercase]]
			}
		}
	}

	on [titlecase] (This) {
		if length < 2 {
			return this[uppercase]
		} else {
			return This[new: length]
			-> [add: buffer[at: 0][uppercase]]
			-> {
				for my i from: 1 upto: _.length {
					this[add: _.buffer[at: i][lowercase]]
				}
			}
		}
	}

	on [capitalize] (This) {
		return this[new] -> {
			if length > 0 {
				this[at: 0] = this[at: 0][uppercase]
			}
		}
	}


	;== Accessing
	
	on [at: index (Int) set: str (Str)] is setter {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			this[Unsafe at: index set: str]
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeAt: index (Int) set: str (Str)] is setter {
		if index < 0 {
			index += length
		}

		if 0 <= index < length {
			this[Unsafe at: index set: str]
		}
	}


	;== Appending

	on [add: str (Str)] (Str) {
		match str.length {
			at 0 {}
			at 1 => this[add: str[at: 0]]
			else => this[Super addAll: str]
		}

		return str
	}

	type Stringy {
		on [Str]
	}
	on [add: value (Stringy)] (Stringy) is inline {
		this[add: value[Str]]

		return value
	}

	type Stringy {
		on [Str]
	}
	type Iter of Iterable[Stringy]
	on [addAll: values (Iter)] (Iter) {
		for my value in: values {
			this[add: value]
		}

		return values
	}
	
	
	;== Prepending
	
	on [prepend: str (Str)] (Str) {
		match str.length {
			at 0 {}
			at 1 => this[prepend: str[at: 0]]
			else => this[Super prependAll: str]
		}

		return str
	}

	type Stringy {
		on [Str]
	}
	on [prepend: value (Stringy)] (Stringy) is inline {
		this[prepend: value[Str]]

		return value
	}

	type Stringy {
		on [Str]
	}
	type Iter of Iterable[Stringy]
	on [prependAll: values (Iter)] (Iter) {
		this[prepend: "" -> {
			for my value in: values {
				this[add: value]
			}
		}]

		return values
	}


	;== Collecting

	on [collect: func (Func[Char, Char, Int])] (This) {
		return This[new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: func[call: _.buffer[at: i], i]]
			}
		}
	}

	on [collectAll: func (Func[Str, Char, Int])] (This) {
		return This[new: length] -> {
			for my i from: 0 upto: _.length {
				this[add: func[call: _.buffer[at: i], i]]
			}
		}
	}

	type T of Iterable[Char]
	on [collectAll: func (Func[T, Char, Int])] (This) {
		return This[new: length] -> {
			for my i from: 0 upto: _.length {
				this[addAll: func[call: _.buffer[at: i], i]]
			}
		}
	}


	;== Collecting *and* filtering

	on [collectIf: func (Func[Maybe[Char], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				}
			}
		}
	}

	on [collectWhile: func (Func[Maybe[Char], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				} else {
					break
				}
			}
		}
	}

	on [collectAllIf: func (Func[Maybe[Str], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				}
			}
		}
	}
	
	type T of Iterable[Char]
	on [collectAllIf: func (Func[Maybe[T], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				}
			}
		}
	}

	on [collectAllWhile: func (Func[Maybe[Str], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my value] {
					this[add: value]
				} else {
					break
				}
			}
		}
	}
	
	type T of Iterable[Char]
	on [collectAllWhile: func (Func[Maybe[T], Char, Int])] (This) {
		return This[new: length // 2] -> {
			for my i from: 0 upto: _.length {
				match func[call: _.buffer[at: i], i] at Maybe[the: my values] {
					this[addAll: values]
				} else {
					break
				}
			}
		}
	}
}

;[
Maybe add:
- tr
- count

Star.Power:
- camelCase/snakeCase/pascalCase/screamingCase/kebabCase/noCase/etc
]