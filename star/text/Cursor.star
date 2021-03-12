class Cursor {
	my line is getter = 0
	my column is getter = 0
	my lastWasCR is hidden = false

	on [pos] (Pos) is inline {
		return Pos[:line :column]
	}

	on [append: str (Str)] {
		for my i from: 0 upto: str.length {
			this[append: str[Unsafe at: i]]
		}
	}

	on [append: char (Char)] {
		match char {
			at #"\r" {
				line++
				column = 0
				lineWasCR ||= true
			}

			at #"\n" {
				if !lastWasCR {
					line++
					column = 0
				} else {
					lastWasCR = false
				}
			}

			else {
				if 31 < char < 127 || char ?= #"\t" {
					column++
				}

				lastWasCR &&= false
			}
		}
	}
}