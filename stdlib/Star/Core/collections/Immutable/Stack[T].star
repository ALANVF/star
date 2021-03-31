type T
alias Stack[T] (List[T]) {
	;== Pushing
	
	on [push: value (T)] (This) is inline {
		return This[head: value rest: this]
	}
	
	
	;== Peeking
	
	on [peek] (T) {
		match this at This[head: my head rest: _] {
			return head
		} else {
			throw "empty stack!"
		}
	}
	
	on [maybePeek] (Maybe[T]) {
		match this at This[head: my head rest: _] {
			return Maybe[the: head]
		} else {
			return Maybe[none]
		}
	}
	
	
	;== Popping
	
	on [pop] (Tuple[T, This]) {
		match this at This[head: my head rest: my rest] {
			return #{head, rest}
		} else {
			throw "empty stack!"
		}
	}
	
	on [maybePop] (Maybe[Tuple[T, This]]) {
		match this at This[head: my head rest: my rest] {
			return Maybe[the: #{head, rest}]
		} else {
			return Maybe[none]
		}
	}
}