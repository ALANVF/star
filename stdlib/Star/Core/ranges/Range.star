;@@ TODO: revisit `Step` typing

type T of ElemWithStep[S] ; this might or might not work. won't know until typechecker is done
type S of Step
protocol Range[T, S] of Iterable[T] {
	my from (T) is getter
	my to (T) is getter
	my step (S) is getter

	
	;== Creating

	on [from: (T) to: (T) by: (S) = S 1] (Range[T, S]) is static is inline is noinherit {
		return Forwards[:from :to :by]
	}
	
	on [after: (T) to: (T) by: (S) = S 1] (Range[T, S]) is static is inline is noinherit {
		return Forwards[:after :to :by]
	}

	on [from: (T) upto: (T) by: (S) = S 1] (Range[T, S]) is static is inline is noinherit {
		return Forwards[:from :upto :by]
	}

	on [after: (T) upto: (T) by: (S) = S 1] (Range[T, S]) is static is inline is noinherit {
		return Forwards[:after :upto :by]
	}

	on [from: (T) downto: (T) by: (S) = S -1] (Range[T, S]) is static is inline is noinherit {
		return Backwards[:from :downto :by]
	}

	on [after: (T) downto: (T) by: (S) = S -1] (Range[T, S]) is static is inline is noinherit {
		return Backwards[:after :downto :by]
	}


	;== Observing

	on [extent] (T) is getter is inline {
		return this[max] - this[min]
	}

	on [length] (Int) is getter {
		return [this[extent] // step Int] + 1
	}

	on [max] (T)
	
	on [min] (T)

	on [sum] (T) {
		my sum = T 0
		
		for my value from: from to: to by: step {
			sum += value
		}

		return sum
	}


	;== Accessing

	on [at: index (Int)] (T) {
		if 0 <= index <= this.length {
			return from + step * index
		} else {
			throw IndexError[at: index]
		}
	}

	on [maybeAt: index (Int)] (Maybe[T]) {
		if 0 <= index <= this.length {
			return Maybe[the: from + step * index]
		} else {
			return Maybe[none]
		}
	}


	;== Testing

	on [contains: value (T)] (Bool)
	on [contains: range (Range[T, S])] (Bool)

	on [intersects: range (Range[T, S])] (Bool)


	;== Collecting
	
	type U
	on [collect: func (Func[U, T])] (Array[U]) is inline {
		return Array[new: this.length] -> {
			for my value from: from to: to by: step {
				this[add: func[call: value]]
			}
		}
	}
	type U
	on [collect: func (Func[U, T, Int])] (Array[U]) is inline {
		return Array[new: this.length] -> {
			my i = 0
			for my value from: from to: to by: step {
				this[add: func[call: value, i++]]
			}
		}
	}


	;== Filtering
	
	on [keepIf: func (Func[Bool, T])] (Array[T]) is inline {
		return Array[T][new: this.length // 2] -> {
			for my value from: from to: to by: step {
				if func[call: value] {
					this[add: value]
				}
			}
		}
	}
	on [keepIf: func (Func[Bool, T, Int])] (Array[T]) is inline {
		return Array[T][new: this.length // 2] -> {
			my i = 0
			for my value from: from to: to by: step {
				if func[call: value, i++] {
					this[add: value]
				}
			}
		}
	}


	;== Collecting *and* filtering
	
	type U
	on [collectIf: func (Func[Maybe[U], T])] (Array[U]) is inline {
		return Array[U][new: this.length // 2] -> {
			for my value from: from to: to by: step {
				match func[call: value] at Maybe[the: my value'] {
					this[add: value']
				}
			}
		}
	}
	type U
	on [collectIf: func (Func[Maybe[U], T, Int])] (Array[U]) is inline {
		return Array[U][new: this.length // 2] -> {
			my i = 0
			for my value from: from to: to by: step {
				match func[call: value, i++] at Maybe[the: my value'] {
					this[add: value']
				}
			}
		}
	}


	;== Iterating

	on [each: func (Func[Void, T])] is inline {
		for my value from: from to: to by: step {
			func[call: value]
		}
	}
	on [each: func (Func[Void, T, Int])] is inline {
		my i = 0
		for my value from: from to: to by: step {
			func[call: value, i++]
		}
	}


	;== Reversing

	;on [reverse] (Range[T, S])


	;== Transforming

	;operator `&` [range (Range[T, S])] (Range[T, S])
	;operator `|` [range (Range[T, S])] (Range[T, S])
	;operator `^` [range (Range[T, S])] (Range[T, S])

	;operator `-` (Range[T, S])

	;operator `+` [offset (T)] (Range[T, S])
	;operator `-` [offset (T)] (Range[T, S])


	;== Converting

	on [Array[T]] {
		return Array[new: this.length] -> {
			for my value from: from to: to by: step {
				this[add: value]
			}
		}
	}
}


type T of Step
alias Range[T] = Range[T, T]

type T of Char
alias Range[T] = Range[T, Int]