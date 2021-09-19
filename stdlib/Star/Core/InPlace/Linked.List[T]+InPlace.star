type T
category InPlace for Linked.List[T] {
	;== Collecting

	on [collect: func (Func[T, T, Int])] {
		for my i, my link in: this[links] {
			link.value = func[call: link.value, i]
		}
	}


	;== Filtering

	on [keepIf: func (Func[Bool, T, Int])] {
		for my i, my link in: this[links] {
			if !func[call: link.value, i] {
				this[removeLink: link]
				length--
			}
		}
	}

	on [keepWhile: func (Func[Bool, T, Int])] {
		for my i, my link in: this[links] {
			if !func[call: link.value, i] {
				this[removeFromLink: link]
				length = i
				break
			}
		}
	}


	;== Collecting *and* filtering

	on [collectIf: func (Func[Maybe[T], T, Int])] {
		for my i, my link in: this[links] {
			match func[call: link.value, i] at Maybe[the: my value] {
				link.value = value
			} else {
				this[removeLink: link]
				length--
			}
		}
	}

	on [collectWhile: func (Func[Maybe[T], T, Int])] {
		for my i, my link in: this[links] {
			match func[call: link.value, i] at Maybe[the: my value] {
				link.value = value
			} else {
				this[removeFromLink: link]
				length = i
				break
			}
		}
	}


	;== Reversing

	on [reverse] {
		if length > 1 {
			my link = head.next
			my last = tail.prev
			while last != link {
				last.prev.next = tail
				link[insertPrev: last]
				last = tail.prev
			}
		}
	}


	;== Rotating
	
	on [rotate: offset (Int)] {
		if offset < 0 {
			this[rotateLeft: -offset]
		} else {
			this[rotateRight: offset]
		}
	}

	on [rotateLeft: offset (Int)] {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset ?= 0 || length < 2 {}
			else => for _ from: 0 times: offset {
				my link = head.next[Unsafe Value]
				head.next = link.next
				tail[insertPrev: link]
			}
		}
	}

	on [rotateRight: offset (Int)] {
		case {
			at offset < 0 => throw "Invalid offset"
			at offset ?= 0 || length < 2 {}
			else => for _ from: 0 times: offset {
				my link = tail.prev[Unsafe Value]
				tail.prev = link.prev
				head[insertNext: link]
			}
		}
	}
}