type T
kind Maybe[T] of Iterable[T] {
	has [none]
	has [the: value (T)]

	
	on [value] (T) is getter {
		match this at Maybe[the: my value] {
			return value
		} else {
			throw "Empty value!"
		}
	}

	on [length] (Int) is getter {
		return [this != This[none] Int]
	}

	
	type U
	on [collect: fn (Func[U, T])] (Maybe[U]) {
		match this at Maybe[the: my value] {
			return Maybe[the: fn[call: value]]
		} else {
			return Maybe[none]
		}
	}

	type U
	on [collectAll: fn (Func[Maybe[U], T])] (Maybe[U]) {
		match this at Maybe[the: my value] {
			return fn[call: value]
		} else {
			return Maybe[none]
		}
	}

	on [keepIf: fn (Func[Bool, T])] (Maybe[T]) {
		match this at Maybe[the: my value] {
			if fn[call: value] {
				return Maybe[the: value]
			}
		}
		
		return Maybe[none]
	}

	on [orElse: default (T)] (T) {
		match this at Maybe[the: my value] {
			return value
		} else {
			return default
		}
	}
	
	on [contains: value (T)] (Bool) {
		match this at Maybe[the: value] {
			return true
		} else {
			return false
		}
	}

	; ...
	

	operator `!` (Bool) => return this ?= This[none]
	operator `?` (Bool) => return this != This[none]


	on [Bool] => return this != This[none]

	on [Iterator[T]] {
		match this at Maybe[the: my value] {
			return OnceIterator[new: value]
		} else {
			return EmptyIterator[new]
		}
	}
	
	type T' if Power.Castable[T, T']?
	on [Maybe[T']] {
		match this at Maybe[the: my value] {
			return Maybe[the: value[T']]
		} else {
			return Maybe[none]
		}
	}
}

type T
kind Maybe[Maybe[T]] {
	on [flatten] (Maybe[T]) {
		match this at Maybe[the: my maybe] {
			return maybe
		} else {
			return Maybe[none]
		}
	}
}


type T
category Unsafe for Maybe[T] {
	on [value] (T) is getter is inline is asm {
		return #kind_slot #{this, 0}[T]
	}

	on [length] (Int) is getter is inline is asm {
		return #kind_id this
	}
}