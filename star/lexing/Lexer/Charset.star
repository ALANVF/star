use UInt8 from: Native

class Charset is hidden Lexer {
	my bytes (Array[UInt8]) is hidden
	my size (Int) is hidden

	init [new: size (Int)] {
		bytes = Array[fill: size with: UInt8 0]
		this.size = size
	}

	type Chars of Iterable[Char] {
		on [max] (Char)
	}
	init [new: chars (Chars)] {
		#init_this This[new: (chars[max][Int] >> 3) + 1]

		for my char in: chars {
			bytes[at: char[Int] >> 3] = char[Charset UInt8]
		}
	}

	on [new] (Charset) {
		return Charset[bytes: bytes[new] :size]
	}

	on [has: char (Char)] (Bool) {
		my i = char[Int] >> 3

		return i < size && char[Charset UInt8] & bytes[at: i] != UInt8 0
	}

	on [add: char (Char)] {
		my i = char[Int] >> 3

		if i >= size {
			my extraSize = i - size

			for _ from: 0 times: extraSize => bytes[add: 0]

			bytes[at: i] = char[Charset UInt8]
		} else {
			bytes[at: i] |= char[Charset UInt8]
		}
	}

	on [addAll: chars (Iterable[Char])] {
		for my char in: chars => this[add: char]
	}

	on [remove: char (Char)] {
		my i = char[Int] >> 3
		
		if i < size {
			bytes[at: i] &= ~char[Charset UInt8]
		}
	}

	on [removeAll: chars (Iterable[Char])] {
		for my char in: chars => this[remove: char]
	}

	on [at: char (Char)] (Bool) is inline {
		return this[has: char]
	}

	on [at: char (Char) set: status (Bool)] is setter {
		if status {
			this[add: char]
		} else {
			this[remove: char]
		}
	}

	operator `|` [other (Charset)] (Charset) {
		my out = Charset[new: size[max: other.size]]
		
		out.bytes[from: 0 by: size] = bytes

		for my char from: #"\0" upto: [other.size << 3 Char] {
			out[at: char] ||= other[at: char]
		}

		return out
	}

	operator `|` [chars (Iterable[Char])] (Charset) {
		return this[new]
		-> [addAll: chars]
	}

	operator `|` [char (Char)] (Charset) {
		return this[new]
		-> [add: char]
	}
}

category Charset for Char is hidden {
	on [UInt8] {
		return UInt8 1 << (UInt8 7 - (this[UInt8] & UInt8 7))
	}
}