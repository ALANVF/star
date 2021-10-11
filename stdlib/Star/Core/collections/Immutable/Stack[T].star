type T
alias Stack[T] (List[T]) {
	;== Pushing
	
	on [push: value (T)] (This) is inline => return This[head: value tail: this]
	
	
	;== Peeking
	
	on [peek] (T) {
		match this at This[head: my head tail: _] {
			return head
		} else {
			throw "empty stack!"
		}
	}
	
	on [maybePeek] (Maybe[T]) {
		match this at This[head: my head tail: _] {
			return Maybe[the: head]
		} else {
			return Maybe[none]
		}
	}
	
	
	;== Popping
	
	on [pop] (Tuple[T, This]) {
		match this at This[head: my head tail: my tail] {
			return #{head, tail}
		} else {
			throw "empty stack!"
		}
	}
	
	on [maybePop] (Maybe[Tuple[T, This]]) {
		match this at This[head: my head tail: my tail] {
			return Maybe[the: #{head, tail}]
		} else {
			return Maybe[none]
		}
	}
}