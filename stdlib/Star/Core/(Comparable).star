protocol Comparable {
	operator `>` [other (This)] (Bool)

	operator `>=` [other (This)] (Bool) {
		return !(this < other)
	}
	
	operator `<` [other (This)] (Bool)
	
	operator `<=` [other (This)] (Bool) {
		return !(this > other)
	}
}