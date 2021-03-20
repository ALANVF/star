type T
category Unsafe for Values[T] {
	;== Accessing
	
	on [at: index (Int)] (T) is inline {
		return buffer[at: index]
	}

	on [at: index (Int) set: value (T)] is setter is inline {
		buffer[at: index] = value
	}
	
	
	;== Removing elements
	
	on [removeAt: index (Int)] (T) {
		my value = buffer[at: index]

		this[after: index moveBy: -1]
		
		return value
	}
}