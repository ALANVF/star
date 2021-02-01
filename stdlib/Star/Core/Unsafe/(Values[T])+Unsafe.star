type T
category Unsafe for Values[T] {
	;== Accessing
	
	on [at: index (Int)] (T) {
		return buffer[at: index]
	}

	on [at: index (Int) set: value (T)] is setter {
		buffer[at: index] = value
	}
}