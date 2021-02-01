type A
type M[A] {
	on [return: (A)] (M[A]) is static

	type B
	on [bind: (Func[M[B], A])] (M[B])

	type B
	on [fmap: (Func[B, A])] (M[B])
}
alias Monad[A] = M[A]

type T
kind Maybe[T] {
	has [none]
	has [some: (T)]

	on [return: value (T)] (Maybe[T]) is static {
		return Maybe[some: value]
	}

	type U
	on [bind: func (Func[Maybe[U], T])] (Maybe[U]) {
		match this {
			at Maybe[none] => return Maybe[none]
			at Maybe[some: my value] => return func[call: value]
		}
	}
	
	type U
	on [fmap: func (Func[U, T])] (Maybe[U]) {
		match this {
			at Maybe[none] => return Maybe[none]
			at Maybe[some: my value] => return Maybe[some: func[call: value]]
		}
	}
}

module Main {
	type T of Monad[Int]
	on [add1: monad (T)] (T) {
		return monad[fmap: $0 + 1]
	}

	on [main] {
		my value = Maybe[some: 5]

		Core[say: value] ;=> Maybe[some: 5]

		value = Main[add1: value]

		Core[say: value] ;=> Maybe[some: 6]
	}
}
