class Reader of Series[Char] {
	on [hasNext] (Bool) is inline {
		return !this[isTail]
	}

	on [eat: char (Char)] (Maybe[Reader]) {
		if this[startsWith: char] {
			return Maybe[the: this[next]]
		} else {
			return Maybe[none]
		}
	}

	on [eat: str (Str)] (Maybe[Reader]) {
		if this[startsWithEach: str] {
			return Maybe[the: this[skip: str.length]]
		} else {
			return Maybe[none]
		}
	}

	on [eat: charset (Charset)] (Maybe[Tuple[Char, Reader]]) {
		if this[hasNext] {
			my char = this[first]

			if charset[has: char] {
				return Maybe[the: #{char, this[next]}]
			}
		}

		return Maybe[None]
	}
}