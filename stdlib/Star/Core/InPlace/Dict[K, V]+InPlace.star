type K
type V
category InPlace for Dict[K, V] {
	;== Collecting
	
	on [collect: func (Func[Tuple[K, V], K, V])] {
		my oldPairs = pairs
		my i = 0
		
		pairs = #[]
		
		try {
			while i < oldPairs.length {
				my pair = oldPairs[Unsafe at: i]
				
				match func[call: pair.key, pair.value] at #{my key, my value} {
					this[at: key] = value
				}
			}
		} catch {
			at my err {
				;-- Make sure to re-add untouched pairs before throwing
				i--
				while i < oldPairs.length {
					my pair = oldPairs[Unsafe at: i++]
					
					this[atNew: pair.key] = pair.value
				}
				
				throw err
			}
		}
	}
	
	
	;== Filtering
	
	on [keepIf: func (Func[Bool, K, V])] {
		my i = 0
		while i < pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if func[call: pair.key, pair.value] {
				i++
			} else {
				pairs[removeAt: i]
			}
		}
	}
	
	on [keepWhile: func (Func[Bool, K, V])] {
		for my i from: 0 upto: pairs.length {
			my pair = pairs[Unsafe at: i]
			
			if !func[call: pair.key, pair.value] {
				pairs[removeFrom: i]
				break
			}
		}
	}
	
	
	;== Collecting *and* filtering
	
	on [collectIf: func (Func[Maybe[K, V], K, V])] {
		my oldPairs = pairs
		my i = 0
		
		pairs = #[]
		
		try {
			while i < oldPairs.length {
				my pair = oldPairs[Unsafe at: i]
				
				match func[call: pair.key, pair.value] at Maybe[the: #{my key, my value}] {
					this[at: key] = value
				}
			}
		} catch {
			at my err {
				;-- Make sure to re-add untouched pairs before throwing
				i--
				while i < oldPairs.length {
					my pair = oldPairs[Unsafe at: i]
					
					this[atNew: pair.key] = pair.value
				}
				
				throw err
			}
		}
	}
	
	on [collectWhile: func (Func[Maybe[K, V], K, V])] {
		my oldPairs = pairs
		my i = 0
		
		pairs = #[]
		
		while i < oldPairs.length {
			my pair = oldPairs[Unsafe at: i]
			
			match func[call: pair.key, pair.value] at Maybe[the: #{my key, my value}] {
				this[at: key] = value
			} else {
				break
			}
		}
	}
}