use Tokens from: Lexer

type T
kind Result[T] {
	has [success: made (T), rest (Tokens)]
	has [failure: begin (Tokens), end (Maybe[Tokens])]
	has [fatal: begin (Tokens), end (Maybe[Tokens])]
	has [eof: begin (Tokens)]
	has [fatalError: (Diagnostic)]
	
	on [updateIfBad: tokens (Tokens)] (This) {
		match this {
			at This[failure: my begin, Maybe[none]] => return This[failure: tokens, Maybe[the: begin]]
			at This[failure: _, my rest] => return This[failure: tokens, rest]
			at This[fatal: my begin, Maybe[none]] => return This[fatal: tokens, Maybe[the: begin]]
		}
	}
	
	on [fatalIfBad: tokens (Tokens)] (This) {
		match this {
			at This[failure: my begin, Maybe[none]] || This[fatal: my begin, Maybe[none]] => return This[fatal: tokens, Maybe[the: begin]]
			at This[failure: _, my rest] => return This[fatal: tokens, rest]
		}
	}
	
	on [fatalIfFailed] (This) {
		match this at This[failure: my begin, my end] {
			return This[fatal: begin, end]
		} else {
			return this
		}
	}
	
	type U
	on [Result[U]] {
		match this at This[success: my made, my rest] {
			return Result[U][success: made[U], rest]
		} else {
			return this[Unsafe Result[U]]
		}
	}
}