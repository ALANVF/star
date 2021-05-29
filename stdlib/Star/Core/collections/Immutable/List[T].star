type T
class ListIterator[T] of Iterator[T] is hidden {
	my list (List[T])
	
	on [next] (Maybe[T]) {
		match list at List[head: my head tail: my tail] {
			list = tail
			return Maybe[the: head]
		} else {
			return Maybe[none]
		}
	}
}

type T
kind List[T] of Iterable[T] {
	type U
	alias This'[U] is hidden = Power.Reapply[This, U]
	
	
	has [nil]
	has [head: (T) tail: (This)]
	
	
	;== Creating
	
	on [new] (This) is static is inline {
		return This[nil]
	}
	
	on [new: values (Iterable[T])] (This) is static {
		return This[new: values[Iterator[T]]]
	}
	
	on [new: iter (Iterator[T])] (This) is static is hidden {
		match iter[next] at Maybe[the: my value] {
			return This[head: value tail: This[new: iter]]
		} else {
			return This[nil]
		}
	}
	
	
	;== Length
	
	on [length] (Int) is getter {
		match this at Maybe[head: _ tail: my tail] {
			return 1 + tail.length
		} else {
			return 0
		}
	}
	
	
	;== Accessing
	
	on [at: index (Int)] (T) {
		if index < 0 {
			index += this.length
		}
		
		my index' = index
		my list = this
		
		while true {
			match #{index', this} {
				at #{_, This[nil]} => IndexError[at: index]
				at #{0, This[head: my head tail: _]} => return head
				at #{_, This[head: _ tail: list = _]} => index'--
			}
		}
	}
	
	on [maybeAt: index (Int)] (Maybe[T]) {
		if index < 0 {
			index += this.length
		}
		
		my index' = index
		my list = this
		
		while true {
			match #{index', this} {
				at #{_, This[nil]} => return Maybe[none]
				at #{0, This[head: my head tail: _]} => return Maybe[the: head]
				at #{_, This[head: _ tail: list = _]} => index'--
			}
		}
	}
	
	
	;== Slicing
	
	on [from: (Int)] (This) {
		if from < 0 {
			from += this.length
		}
		
		my index = from
		my list = this
		
		while true {
			match #{index, this} {
				at #{_, This[nil]} => throw RangeError[:from to: -1]
				at #{0, This[head: _ tail: _]} => return list
				at #{_, This[head: _ tail: list = _]} => index--
			}
		}
	}
	
	on [after: (Int)] (This) is inline {
		return this[from: after + 1]
	}
	
	;... and all other variants
	
	
	;== Removing
	
	on [removeAt: index (Int)] (Tuple[T, This]) {
		if index < 0 {
			index += this.length
		}
		
		try {
			match this[from: index] at #{my value, my rest} {
				return #{value, this[upto: index] + rest}
			}
		} catch {
			at RangeError[Int] => throw IndexError[at: index]
		}
	}
	
	on [maybeRemoveAt: index (Int)] (Maybe[Tuple[T, This]]) {
		if index < 0 {
			index += this.length
		}
		
		try {
			match this[from: index] at #{my value, my rest} {
				return Maybe[the: #{value, this[upto: index] + rest}]
			}
		} catch {
			at RangeError[Int] => return Maybe[none]
		}
	}
	
	
	;== Adding
	
	on [add: value (T)] (This) {
		match this at This[head: my head tail: my tail] {
			return This[:head tail: tail[add: value]]
		} else {
			return This[head: value tail: This[nil]]
		}
	}
	
	
	;== Prepending
	
	on [prepend: value (T)] (This) is inline {
		return This[head: value tail: this]
	}
	
	
	;== Iterating
	
	on [Iterator[T]] is inline {
		return ListIterator[list: this]
	}
	
	on [each: func (Func[Void, T, Int])] {
		my i = 0
		my list = this
		
		while true {
			match list at This[head: my head tail: my tail] {
				func[call: head, i++]
				list = tail
			} else {
				break
			}
		}
	}
	
	
	;== Reversing
	
	on [reverse] (This) {
		my list = this
		my result = This[nil]
		
		while true {
			match list at This[head: my head tail: my tail] {
				result = result[prepend: head]
				list = tail
			} else {
				return result
			}
		}
	}
	
	
	;== Collecting
	
	type U
	on [collect: func (Func[U, T])] (This'[U]) {
		match this at This[head: my head tail: my tail] {
			my head' = func[call: head]
			return tail[collect: func][prepend: head']
		}
	}
	type U
	on [collect: func (Func[U, T, Int])] (This'[U]) {
		my i = 0
		my list = this
		my result = This[nil]
		
		while true {
			match list at This[head: my head tail: my tail] {
				result = result[prepend: func[call: head, i++]]
				list = tail
			} else {
				return result[reverse]
			}
		}
	}
	
	
	;== Filtering
	
	on [keepIf: func (Func[Bool, T])] (This) {
		match this at This[head: my head tail: my tail] {
			if func[call: head] {
				return This[:head tail: tail[keepIf: func]]
			} else {
				return tail[keepIf: func]
			}
		} else {
			return This[nil]
		}
	}
	on [keepIf: func (Func[Bool, T, Int])] (This) {
		my i = 0
		my list = this
		my result = This[nil]
		
		while true {
			match list at This[head: my head tail: my tail] {
				if func[call: head, i++] {
					result = result[prepend: head]
				}
				
				list = tail
			} else {
				return result[reverse]
			}
		}
	}
	
	on [keepWhile: func (Func[Bool, T])] (This) {
		match this at This[head: my head tail: my tail] if func[call: head] {
			return This[:head tail: tail[keepIf: func]]
		} else {
			return This[nil]
		}
	}
	on [keepWhile: func (Func[Bool, T, Int])] (This) {
		my i = 0
		my list = this
		my result = This[nil]
		
		while true {
			match list at This[head: my head tail: my tail] if func[call: head, i++] {
				result = result[prepend: head]
				list = tail
			} else {
				return result[reverse]
			}
		}
	}
	
	
	;== Collecting *and* filtering
	
	type U
	on [collectIf: func (Func[Maybe[U], T])] (This'[U]) {
		match this at This[head: my head tail: my tail] {
			match func[call: head] at Maybe[the: my head'] {
				return This'[U][head: head' tail: tail[collectIf: func]]
			} else {
				return tail[collectIf: func]
			}
		} else {
			return This'[U][nil]
		}
	}
	
	type U
	on [collectIf: func (Func[Maybe[U], T, Int])] (This'[U]) {
		my i = 0
		my list = this
		my result = This'[U][nil]
		
		while true {
			match list at This[head: my head tail: my tail] {
				match func[call: head, i++] at Maybe[the: my value] {
					result = result[prepend: head]
				}
				
				list = tail
			} else {
				return result[reverse]
			}
		}
	}
	
	type U
	on [collectWhile: func (Func[Maybe[U], T])] (This'[U]) {
		match this at This[head: my head tail: my tail] {
			match func[call: head] at Maybe[the: my head'] {
				return This'[U][head: head' tail: tail[collectWhile: func]]
			} else {
				return This'[U][nil]
			}
		} else {
			return This'[U][nil]
		}
	}
	
	type U
	on [collectWhile: func (Func[Maybe[U], T, Int])] (This'[U]) {
		my i = 0
		my list = this
		my result = This'[U][nil]
		
		while true {
			match list at This[head: my head tail: my tail] {
				match func[call: head, i++] at Maybe[the: my value] {
					result = result[prepend: head]
					list = tail
				} else {
					break
				}
			} else {
				break
			}
		}
		
		return result[reverse]
	}
	
	
	;== Concating
	
	operator `+` [other (This)] (This) {
		match this at This[head: my head tail: my tail] {
			return This[:head tail: tail + other]
		} else {
			return other
		}
	}
	
	
	;== Checking
	
	operator `?` (Bool) {
		return this != This[nil]
	}
}