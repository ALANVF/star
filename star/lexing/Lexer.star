class Lexer {
	my hspace is static is readonly = Charset[new: " \t"]
	my vspace is static is readonly = Charset[new: #"\n"[to: #"\r"]]
	my digit is static is readonly = Charset[new: #"0"[to: #"9"]]
	my lower is static is readonly = Charset[new: #"a"[to: #"z"]]
	my upper is static is readonly = Charset[new: #"A"[to: #"Z"]]
	my alpha is static is readonly = upper | lower
	my alnum is static is readonly = alpha | digit | #"_"
	my xdigit is static is readonly = digit | "abcdefABCDEF"
	my hspace_semi is static is readonly = hspace | #";"
	my lower_u is static is readonly = lower | #"_"
	my alpha_u is static is readonly = alpha | #"_"
	my alnum_q is static is readonly = alnum | #"'"
	my single_char is static is readonly = Charset[new: "()[]{}~"]

	my keywords is static is readonly = #(
		"module" => Token[module]->[span: $.0]
		"my" => Token[my]->[span: $.0]
		"on" => Token[on]->[span: $.0]
		"return" => Token[return]->[span: $.0]
		"init" => Token[init]->[span: $.0]
		"deinit" => Token[deinit]->[span: $.0]
		"operator" => Token[operator]->[span: $.0]
		"class" => Token[class]->[span: $.0]
		"alias" => Token[alias]->[span: $.0]
		"type" => Token[type]->[span: $.0]
		"kind" => Token[kind]->[span: $.0]
		"category" => Token[category]->[span: $.0]
		"protocol" => Token[protocol]->[span: $.0]
		"is" => Token[is]->[span: $.0]
		"of" => Token[of]->[span: $.0]
		"use" => Token[use]->[span: $.0]
		"has" => Token[has]->[span: $.0]
		"if" => Token[if]->[span: $.0]
		"orif" => Token[orif]->[span: $.0]
		"else" => Token[else]->[span: $.0]
		"while" => Token[while]->[span: $.0]
		"for" => Token[for]->[span: $.0]
		"do" => Token[do]->[span: $.0]
		"case" => Token[case]->[span: $.0]
		"match" => Token[match]->[span: $.0]
		"at" => Token[at]->[span: $.0]
		"break" => Token[break]->[span: $.0]
		"next" => Token[next]->[span: $.0]
		"throw" => Token[throw]->[span: $.0]
		"try" => Token[try]->[span: $.0]
		"catch" => Token[catch]->[span: $.0]
	)
	my attrs is static is readonly = #(
		"static" => Token[static]->[span: $.0]
		"hidden" => Token[hidden]->[span: $.0]
		"readonly" => Token[readonly]->[span: $.0]
		"friend" => Token[friend]->[span: $.0]
		"unordered" => Token[unordered]->[span: $.0]
		"getter" => Token[getter]->[span: $.0]
		"setter" => Token[setter]->[span: $.0]
		"main" => Token[main]->[span: $.0]
		"inline" => Token[inline]->[span: $.0]
		"noinherit" => Token[noinherit]->[span: $.0]
		"pattern" => Token[pattern]->[span: $.0]
		"asm" => Token[asm]->[span: $.0]
		"native" => Token[native]->[span: $.0]
		"flags" => Token[flags]->[span: $.0]
		"uncounted" => Token[uncounted]->[span: $.0]
		"strong" => Token[strong]->[span: $.0]
		"sealed" => Token[sealed]->[span: $.0]
		"macro" => Token[macro]->[span: $.0]
	)

	my input (Reader)
	my reader (SourceFile)
	my begin = Pos[new]
	my cursor = Cursor[new]
	my tokens (Tokens) = #[]
	
	init [new: source (SourceFile)] {
		this.source = source
		reader = Reader[new: source.text]
	}

	on [tokenize] (Tokens) {
		while reader[hasNext] {
			tokens[add: this[readToken]]
		}

		tokens[Lexer retoken]

		return tokens
	}

	on [here] (Pos) is inline {
		return cursor.pos[new]
	}

	on [span] (Span) is inline {
		return Span[start: begin end: this[here] :source]
	}
}

category Lexer for Tokens is hidden {
	on [retoken] {
		match tokens {
			at #[Token[dot], Token[name: _], ...my rest] => rest[Lexer retoken]
			
			at #[Token[name: "this" span: my span], ...my rest] {
				this[at: 0] = Token[this]->[:span]
				rest[Lexer retoken]
			}
			at #[Token[name: "true" span: my span], ...my rest] {
				this[at: 0] = Token[bool: true :span]
				rest[Lexer retoken]
			}
			at #[Token[name: "false" span: my span], ...my rest] {
				this[at: 0] = Token[bool: false :span]
				rest[Lexer retoken]
			}

			at #[Token[name: "my" span: my span], Token[name: _], ...my rest] {
				this[at: 0] = Token[my][:span]
				rest[Lexer retoken]
			}
			at #[Token[name: "has" span: my span], Token[name: _], ...my rest] {
				this[at: 0] = Token[has][:span]
				rest[Lexer retoken]
			}

			at #[Token[name: "is" span: my span], Token[name: my name span: my span'], ...my rest] {
				this[at: 0] = Token[is][:span]
				
				match Lexer.attrs[maybeAt: name] at Maybe[the: my token] {
					this[at: 1] = token[call: span']
				}

				rest[Lexer retoken]
			}
			at #[Token[name: my name span: my span], ...my rest] if Lexer.keywords[hasKey: name] {
				this[at: 0] = Lexer.keywords[at: name][call: span']
				rest[Lexer retoken]
			}

			at #[Token[str: my segs], ...my rest] {
				for my seg in: segs {
					match seg at StrSegment[code: my code] {
						code[Lexer retoken]
					}
				}

				rest[Lexer retoken]
			}

			at #[_, ...my rest] => rest[Lexer retoken]
			at #[] {}
		}
	}
}