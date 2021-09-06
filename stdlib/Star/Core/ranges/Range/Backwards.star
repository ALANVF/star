type T of ElemWithStep[S]
type S of Step
class BackwardsIterator[T, S] of Iterator[T] is hidden {
	my from (T)
	my to (T)
	my step (S)

	on [next] (Maybe[T]) {
		if from < to {
			return Maybe[none]
		} else {
			my from' = from
			
			from += step

			return Maybe[the: from']
		}
	}
}

type T of ElemWithStep[S]
type S of Step
class Backwards[T, S] of Range[T, S] is hidden Range {
	;== Creating

	init [from: (T) downto: (T) by: (S)] {
		if from >= downto && by < S 0 {
			this.from = from
			to = downto
			step = by
		} else {
			throw RangeError[:from to: downto]
		}
	}

	init [after: (T) downto: (T) by: (S)] {
		from = after + by
		
		if from >= downto && by < S 0 {
			to = downto
			step = by
		} else {
			throw RangeError[:from to: downto]
		}
	}


	;== Observing

	on [max] (T) is inline {
		return from
	}
	
	on [min] (T) is inline {
		return to
	}


	;== Testing

	on [contains: value (T)] (Bool) is inline {
		return to <= value <= from
	}
	on [contains: range (Range[T, S])] (Bool) {
		return to <= range[min] && range[max] <= from
	}

	on [intersects: range (Range[T, S])] (Bool) {
		return to <= range[min] <= from || to <= range[max] <= from
	}


	;== Iterating

	on [Iterator[T]] is inline {
		return BackwardsIterator[:from :to :step]
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
}