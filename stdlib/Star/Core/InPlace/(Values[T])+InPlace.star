type T
category InPlace for Values[T] {
	on [reverse] {
		match length {
			at 0 || 1 => {}
			
			at 2 {
				my first = buffer[at: 0]

				buffer[at: 0] = buffer[at: 1]
				buffer[at: 1] = first
			}

			else {
				my middle = length // 2
				my left = middle - length % 2
				my right = middle + 1

				while left >= 0 {
					my leftValue = buffer[at: left]
					
					buffer[at: left--] = buffer[at: right]
					buffer[at: right++] = leftValue
				}
			}
		}
	}
}