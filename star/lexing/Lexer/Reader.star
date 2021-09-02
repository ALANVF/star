class Reader of Series[Char] is hidden Lexer {
	my cursor is getter = Cursor[new]

	on [hasNext] (Bool) is inline {
		return !this[isTail]
	}

	on [peek: char (Char)] (Bool) {
		return this[startsWith: char]
	}

	on [peek: str (Str)] (Bool) {
		return this[startsWithEach: str]
	}

	on [peek: charset (Charset)] (Bool) {
		return this[hasNext] && charset[has: this[first]]
	}

	on [at: index (Int) peek: charset (Charset)] (Bool) {
		return offset + index < length && charset[has: this[at: index]]
	}


	on [peekNot: char (Char)] (Bool) is inline {
		return !this[peek: char]
	}

	on [peekNot: str (Str)] (Bool) is inline {
		return !this[peek: char]
	}

	on [peekNot: charset (Charset)] (Bool) {
		return this[hasNext] && !charset[has: this[first]]
	}

	on [at: index (Int) peekNot: charset (Charset)] (Bool) {
		return offset + index < length && !charset[has: this[at: index]]
	}
	

	on [eat] (Char) {
		my char = this[first]

		offset++
		cursor[append: char]

		return char
	}

	on [eat: char (Char)] (Bool) {
		if this[startsWith: char] {
			offset++
			cursor[append: char]
			return true
		} else {
			return false
		}
	}

	on [eat: str (Str)] (Bool) {
		if this[startsWithEach: str] {
			offset += str.length
			cursor[append: str]
			return true
		} else {
			return false
		}
	}

	on [eat: charset (Charset)] (Maybe[Char]) {
		if this[hasNext] {
			my char = this[first]

			if charset[has: char] {
				offset++
				cursor[append: char]
				return Maybe[the: char]
			}
		}

		return Maybe[None]
	}

	on [next] (This) {
		cursor[append: this[first]]
		offset++

		return this
	}

	on [skip: by (Int)] (This) {
		if 0 <= offset + by < buffer.length {
			my offset' = offset + by

			if by ?= 1 {
				cursor[append: this[first]]
			} else {
				for my i from: offset upto: offset' {
					cursor[append: buffer[Unsafe at: i]]
				}
			}

			offset = offset'
		}

		return this
	}

	on [skip: other (This)] (This) {
		if !this[sameSeries: other] {
			throw "Error!"
		}

		for my i from: offset upto: other.offset {
			cursor[append: buffer[Unsafe at: i]]
		}

		offset = other.offset
	}
}