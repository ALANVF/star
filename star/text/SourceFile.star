class SourceFile {
	my path (Str)
	my fullPath (Str)
	my text (Str)
	my lineStarts (Array[Int]) is hidden

	init [path: (Str) text: (Str)] {
		this.path = path
		fullPath = IO[fullPath: path]
		this.text = text
		lineStarts = #[0] -> {
			my cursor = Cursor[new]
			my lastLine = 0
			
			for my i, my char in: text {
				cursor[append: char]

				if cursor.pos.column ?= 0 {
					if cursor.pos.line != lastLine {
						this[add: i + 1]
						lastLine = cursor.pos.line
					} else {
						this[at: -1] = i + 1
					}
				}
			}
		}
	}

	on [lineCount] (Int) is getter {
		return lineStarts.length
	}

	on [isReal] (Bool) {
		return IO.File[exists: fullPath]
	}

	on [lineIndexToTextIndex: index (Int)] (Int) is hidden {
		if index >= lineStarts.length {
			return text.length
		} else {
			return lineStarts[at: index]
		}
	}

	on [atLine: index (Int)] (Str) {
		return text[
			from: this[lineIndexToTextIndex: index]
			to: this[lineIndexToTextIndex: index + 1]
		]
	}

	operator `?=` [other (SourceFile)] (Bool) {
		return fullPath ?= other.fullPath
	}

	on [Str] {
		return path
	}
}