protocol Comparable {
	operator `>` [other (This)] (Bool)
	operator `>=` [other (This)] (Bool) => return !(this < other)
	
	operator `<` [other (This)] (Bool)
	operator `<=` [other (This)] (Bool) => return !(this > other)

	on [min: other (This)] (This) {
		return [this < other yes: this no: other]
	}

	on [max: other (This)] (This) {
		return [this > other yes: this no: other]
	}

	on [min: (This) max: (This)] (This) {
		return [this < min yes: min no: [this > max yes: max no: this]]
	}
}