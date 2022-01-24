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
			else => return this
		}
	}
	
	on [fatalIfBad: tokens (Tokens)] (This) {
		match this {
			at This[failure: my begin, Maybe[none]] || This[fatal: my begin, Maybe[none]] => return This[fatal: tokens, Maybe[the: begin]]
			at This[failure: _, my rest] => return This[fatal: tokens, rest]
			else => return this
		}
	}
	
	on [fatalIfFailed] (This) {
		match this at This[failure: my begin, my end] {
			return This[fatal: begin, end]
		} else {
			return this
		}
	}
	
	;@@ TODO: do we really need this? it's only used once in Parser+Type#finishMsg:
	type U if Power.Castable[T, U]?
	on [Result[U]] {
		match this at This[success: my made, my rest] {
			return Result[U][success: made[U], rest]
		} else {
			return this[Unsafe Result[U]]
		}
	}
	type U if !Power.Castable[T, U]
	on [Result[U]] {
		match this at This[success: my made, my rest] {
			throw "Cannot convert type!"
		} else {
			return this[Unsafe Result[U]]
		}
	}
}

type T
type U if U != T
category Result[U] for Result[T] {
	on [updateIfBad: (Tokens)] (Result[U]) is inline {
		return this[Result[U]][:updateIfBad]
	}
	
	on [fatalIfBad: (Tokens)] (Result[U]) is inline {
		return this[Result[U]][:fatalIfBad]
	}
	
	on [fatalIfFailed] (Result[U]) is inline {
		return this[Result[U]][fatalIfFailed]
	}
}