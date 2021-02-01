category InPlace for Str {
	;== Casing
	
	on [lowercase] {
		for my i from: 0 upto: length {
			buffer[at: i] = buffer[at: i][lowercase]
		}
	}

	on [uppercase] {
		for my i from: 0 upto: length {
			buffer[at: i] = buffer[at: i][uppercase]
		}
	}

	on [titlecase] {
		if length < 2 {
			this[InPlace uppercase]
		} else {
			buffer[at: 0] = buffer[at: 0][uppercase]

			for my i from: 1 upto: length {
				buffer[at: i] = buffer[at: i][lowercase]
			}
		}
	}

	on [capitalize] {
		if length > 0 {
			buffer[at: 0] = buffer[at: 0][uppercase]
		}
	}
}
