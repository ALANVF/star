class Cursor {
	my pos = Pos[new]
	my lastWasCR is hidden = false

	init [new: pos (Pos)] {
		this.pos = pos
	}

	on [append: str (Str)] {
		for my char in: str {
			this[append: char]
		}
	}

	on [append: char (Char)] {
		match char {
			at #"\r" {
				pos = pos[newline]
				lineWasCR = true
			}

			at #"\n" {
				if !lastWasCR {
					pos = pos[newline]
				} else {
					lastWasCR = false
				}
			}

			else {
				if 31 < char < 127 || char ?= #"\t" {
					pos = pos[advance]
				}

				lastWasCR = false
			}
		}
	}
}