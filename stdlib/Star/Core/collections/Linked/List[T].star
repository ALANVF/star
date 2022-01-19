type T
class List[T] of Positional[T] {
	my head (Head[T]) is hidden
	my tail (Tail[T]) is hidden
	my length is getter = 0 

	
	;== Creating

	init [new] {
		head = Head[new]
		tail = Tail[new]
		-> prev = head
	}


	;== Copying

	on [new] (This) {
		my result = This[new]

		for my value in: this {
			result[add: value]
		}

		return result
	}


	;== Accessing

	on [linkAt: index (Int)] (Value[T]) is hidden is inline {
		my link (Value[T])
		
		if index <= length // 2 {
			link = head.nextValue
			for _ from: 0 upto: index {
				link = link.nextValue
			}
		} else {
			link = tail.prevValue
			for _ after: length downto: index {
				link = link.prevValue
			}
		}
		
		return link
	}
	
	on [at: index (Int)] (T) {
		if index < 0 => index += length

		if 0 <= index < length {
			return this[linkAt: index].value
		} else {
			throw IndexError[at: index]
		}
	}

	on [at: index (Int) set: value (T)] is setter {
		if index < 0 => index += length

		if 0 <= index < length {
			this[linkAt: index].value = value
		} else {
			throw IndexError[at: index]
		}
	}


	;== Slicing
	
	on [from: (Int)] (This) {
		if from < 0 => from += length

		if 0 <= from < length {
			my result = This[new]
			
			if from <= length // 2 {
				my link = head.nextValue

				for _ from: 0 upto: from {
					link = link.nextValue
				}

				for _ from: from upto: length {
					result[add: link.value]
					link = link.nextValue
				}
			} else {
				my link = tail.prevValue
				for _ after: length downto: from {
					result[prepend: link.value]
					link = link.prevValue
				}
			}

			return result
		} else {
			throw RangeError[:from to: length - 1]
		}
	}
	on [from: (Int) set: values (This)] is setter {
		if from < 0 => from += length

		if 0 <= from < length {
			if length ?= 0 {
				this[addAll: values]
			} else {
				my link = this[linkAt: from][HasPrev[T]]

				for my value in: values {
					if link ?= tail {
						this[add: value]
					} else {
						my vlink = link[Unsafe Value[T]]
						-> value = value

						link = vlink.next
					}
				}
			}
		} else {
			throw RangeError[:from to: length - 1]
		}
	}
	
	on [to: (Int)] (This) {
		if to < 0 => to += length

		if 0 <= to < length {
			my result = This[new]
			
			my link = head.nextValue
			for _ from: 0 to: to {
				result[add: link.value]
				link = link.nextValue
			}

			return result
		} else {
			throw RangeError[from: 0 :to]
		}
	}
	on [to: (Int) set: values (This)] is setter {
		if to < 0 => to += length

		if 0 <= to < length {
			if length ?= 0 {
				this[addAll: values]
			} else {
				my link = head.next

				for my i, my value in: values {
					if link ?= tail || i > to {
						link[insertPrev: Value[T][:value]]
						length++
					} else {
						my vlink = link[Unsafe Value[T]]
						-> value = value

						link = vlink.next
					}
				}
			}
		} else {
			throw RangeError[from: 0 :to]
		}
	}
	
	on [from: (Int) to: (Int)] (This) {
		if from < 0 => from += length
		if to < 0 => to += length

		if 0 <= from <= to < length {
			my result = This[new]
			
			if from <= length // 2 {
				my link = head.nextValue

				for _ from: 0 upto: from {
					link = link.nextValue
				}

				for _ from: from to: to {
					result[add: link.value]
					link = link.nextValue
				}
			} else {
				my link = tail.prevValue
				for _ from: to downto: from {
					result[prepend: link.value]
					link = link.prevValue
				}
			}

			return result
		} else {
			throw RangeError[:from :to]
		}
	}
	on [from: (Int) to: (Int) set: values (This)] is setter {
		if from < 0 => from += length
		if to < 0 => to += length

		if 0 <= from <= to < length {
			if length ?= 0 {
				this[addAll: values]
			} else {
				my link = this[linkAt: from][HasPrev[T]]
				
				; good place to have a parallel loop?
				my i = from
				for my value in: values {
					if link ?= tail || i > to {
						this[add: value]
					} else {
						my vlink = link[Unsafe Value[T]]
						-> value = value

						link = vlink.next
						i++
					}
				}
			}
		} else {
			throw RangeError[:from :to]
		}
	}


	;== Adding

	on [add: value (T)] (T) {
		tail[insertPrev: Value[T][:value]]
		length++
		return value
	}

	
	;== Prepending

	on [prepend: value (T)] (T) {
		head[insertNext: Value[T][:value]]
		length++
		return value
	}
	
	type Iter of Iterable[T]
	on [prependAll: values (Iter)] (Iter) {
		my link = head.next
		for my value in: values {
			link[insertPrev: Value[T][:value]]
			length++
		}

		return values
	}


	;== Removing values

	on [removeLink: link (Value[T])] is inline {
		link.prev.next = link.next
	}

	on [removeAt: index (Int)] (T) {
		if index < 0 => index += length

		if 0 <= index < length {
			my link = this[linkAt: index]
			-> prev = link.next
			
			length--

			return link.value
		} else {
			throw IndexError[at: index]
		}
	}
	
	
	;== Removing sections

	on [removeFromLink: link (Value[T])] is inline {
		link.prev.next = tail
	}

	on [removeFrom: from (Int)] (This) {
		if from < 0 => from += length

		if 0 <= from < length {
			my res = this[:from]

			this[linkAt: from].prev.next = tail
			length -= from

			return res
		} else {
			throw RangeError[:from to: -1]
		}
	}
	
	on [removeTo: to (Int)] (This) {
		if to < 0 => to += length

		if 0 <= to < length {
			my res = this[:to]

			this[linkAt: to].next.prev = head
			length -= to + 1

			return res
		} else {
			throw RangeError[from: 0 :to]
		}
	}
	
	on [removeFrom: from (Int) to: (Int)] (This) {
		if from < 0 => from += length
		if to < 0 => to += length

		if 0 <= from <= to < length {
			my res = this[:from :to]

			my begin, my end, do {
				my link = this[linkAt: from]

				begin = link

				for _ from: from upto: to {
					link = link.nextValue
				}

				end = link
			}

			begin.prev.next = end.next
			length -= to - from

			return res
		} else {
			throw RangeError[:from :to]
		}
	}


	;== Clearing
	
	on [clear] {
		length = 0
		head.next = tail
	}


	;== Iterating

	on [Iterator[T]] is inline => return ListIterator[link: head.next :tail]

	class Links[T] of Iterable[Value[T]] is hidden {
		my head (Head[T])
		my tail (Tail[T])
		on [Iterator[Value[T]]] is inline => return LinkIterator[link: head :tail]
	}
	on [links] (Links[T]) is hidden is inline => return Links[:head :tail]


	;== Reversing

	on [reverse] (This) {
		my result = This[new]

		for my value in: this {
			result[prepend: value]
		}

		return result
	}


	;== Rotating

	on [rotateLeft: (Int)] (This) is inline {
		return this[new]
		-> [InPlace :rotateLeft]
	}

	on [rotateRight: (Int)] (This) is inline {
		return this[new]
		-> [InPlace :rotateRight]
	}


	;== Comparing
	
	operator `?=` [other (This)] (Bool) {
		return length ?= other.length && (length ?= 0 || {
			my link1 = head.nextValue
			my link2 = other.head.nextValue
			for _ from: 0 upto: length {
				if link1.value != link2.value {
					return false
				} else {
					link1 = link1.nextValue
					link2 = link2.nextValue
				}
			}
			
			return true
		})
	}
	
	operator `!=` [other (This)] (Bool) {
		return length != other.length || (length != 0 && {
			my link1 = head.nextValue
			my link2 = other.head.nextValue
			for _ from: 0 upto: length {
				if link1.value != link2.value {
					return true
				} else {
					link1 = link1.nextValue
					link2 = link2.nextValue
				}
			}
			
			return false
		})
	}


	;== Converting

	type T' if Power.Castable[T, T']?
	on [List[T']] {
		my result = List[new]
		
		for my value in: this {
			result[add: value[T']]
		}
		
		return result
	}

	on [Immutable.List[T]] => return head[Immutable.List[T]]

	on [Array[T]] {
		return Array[new: length]
		-> [addAll: this]
	}

	;[on [Dict[Int, T]] { ... }]
}

type T of Comparable
class List[T] of Comparable {
	;== Comparing
	
	;@@ FIX: buggy due to bad typechecker
	;[operator `>` [other (This)] (Bool) {
		match this.length {
			at _ > other.length => return true
			at _ < other.length => return false
			at 0 => return false
			else {
				my link1 = head.nextValue
				my link2 = other.head.nextValue
				for _ from: 0 upto: length {
					if link1.value <= link2.value {
						return false
					} else {
						link1 = link1.nextValue
						link2 = link2.nextValue
					}
				}
				
				return true
			}
		}
	}
	
	operator `>=` [other (This)] (Bool) {
		match this.length {
			at _ > other.length => return true
			at _ < other.length => return false
			at 0 => return true
			else {
				my link1 = head.nextValue
				my link2 = other.head.nextValue
				for _ from: 0 upto: length {
					if link1.value < link2.value {
						return false
					} else {
						link1 = link1.nextValue
						link2 = link2.nextValue
					}
				}
				
				return true
			}
		}
	}
	
	operator `<` [other (This)] (Bool) {
		match this.length {
			at _ < other.length => return true
			at _ > other.length => return false
			at 0 => return false
			else {
				my link1 = head.nextValue
				my link2 = other.head.nextValue
				for _ from: 0 upto: length {
					if link1.value >= link2.value {
						return false
					} else {
						link1 = link1.nextValue
						link2 = link2.nextValue
					}
				}
				
				return true
			}
		}
	}
	
	operator `<=` [other (This)] (Bool) {
		match this.length {
			at _ < other.length => return true
			at _ > other.length => return false
			at 0 => return true
			else {
				my link1 = head.nextValue
				my link2 = other.head.nextValue
				for _ from: 0 upto: length {
					if link1.value > link2.value {
						return false
					} else {
						link1 = link1.nextValue
						link2 = link2.nextValue
					}
				}
				
				return true
			}
		}
	}]
	
	
	;== Querying
	
	on [min] (T) is inline => return this[Super[Collection[T]] min]
	on [max] (T) is inline => return this[Super[Collection[T]] max]
}

type T
type U of Iterable[T]
class List[U] {
	on [flatten] (This[T]) {
		my result = This[T][new]

		for my value in: this {
			result[addAll: value]
		}

		return result
	}
}


type T
category Unsafe for List[T] {
	;== Removing values

	on [removeAt: index (Int)] (T) {
		my link = this[linkAt: index]
		-> prev = link.next
		
		length--

		return link.value
	}
}