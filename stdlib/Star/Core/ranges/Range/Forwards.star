type T of ElemWithStep[S]
type S of Step
class ForwardsIterator[T, S] of Iterator[T] is hidden {
	my from (T)
	my to (T)
	my step (S)

	on [next] (Maybe[T]) {
		if from > to {
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
class Forwards[T, S] of Range[T, S] is hidden Range {
	;== Creating

	init [from: (T) to: (T) by: (S)] {
		if from <= to && by > S 0 {
			this.from = from
			this.to = to
			step = by
		} else {
			throw RangeError[:from :to]
		}
	}
	
	init [after: (T) to: (T) by: (S)] {
		from = after + by

		if from <= to && by > S 0 {
			this.to = to
			step = by
		} else {
			throw RangeError[:from :to]
		}
	}

	init [from: (T) upto: (T) by: (S)] {
		if from <= upto && by > S 0 {
			this.from = from
			to = upto + by
			step = by
		} else {
			throw RangeError[:from :to]
		}
	}
	
	init [after: (T) upto: (T) by: (S)] {
		from = after + by
		to = upto - by

		if from <= to && by > S 0 {
			step = by
		} else {
			throw RangeError[:from :to]
		}
	}


	;== Observing

	on [max] (T) is inline => return to
	
	on [min] (T) is inline => return from


	;== Testing

	on [contains: value (T)] (Bool) is inline {
		return from <= value <= to
	}
	on [contains: range (Range[T, S])] (Bool) {
		return from <= range[min] && range[max] <= to
	}

	on [intersects: range (Range[T, S])] (Bool) {
		return from <= range[min] <= to || from <= range[max] <= to
	}


	;== Iterating

	on [Iterator[T]] is inline => return ForwardsIterator[:from :to :step]


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