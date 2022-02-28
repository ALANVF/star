class Eof is hidden {}

class Lexer {
	my hspace is static is readonly = Charset[new: " \t"]
	my vspace is static is readonly = Charset[new: #"\n"[to: #"\r"]]
	my digit is static is readonly = Charset[new: #"0"[to: #"9"]]
	my lower is static is readonly = Charset[new: #"a"[to: #"z"]]
	my upper is static is readonly = Charset[new: #"A"[to: #"Z"]]
	my alpha is static is readonly = upper | lower
	my alnum is static is readonly = alpha | digit | #"_"
	my xdigit is static is readonly = digit | "abcdefABCDEF"
	my odigit is static is readonly = Charset[new: #"0"[to: #"7"]]
	my lower_u is static is readonly = lower | #"_"
	my alpha_u is static is readonly = alpha | #"_"
	my alnum_q is static is readonly = alnum | #"'"
	
	my keywords is static is readonly = #(
		"module" => Token[module: Span$.0]
		"my" => Token[my: Span$.0]
		"on" => Token[on: Span$.0]
		"return" => Token[return: Span$.0]
		"init" => Token[init: Span$.0]
		"deinit" => Token[deinit: Span$.0]
		"operator" => Token[operator: Span$.0]
		"class" => Token[class: Span$.0]
		"alias" => Token[alias: Span$.0]
		"type" => Token[type: Span$.0]
		"kind" => Token[kind: Span$.0]
		"category" => Token[category: Span$.0]
		"protocol" => Token[protocol: Span$.0]
		"is" => Token[is: Span$.0]
		"of" => Token[of: Span$.0]
		"use" => Token[use: Span$.0]
		"has" => Token[has: Span$.0]
		"if" => Token[if: Span$.0]
		"else" => Token[else: Span$.0]
		"while" => Token[while: Span$.0]
		"for" => Token[for: Span$.0]
		"do" => Token[do: Span$.0]
		"case" => Token[case: Span$.0]
		"match" => Token[match: Span$.0]
		"at" => Token[at: Span$.0]
		"break" => Token[break: Span$.0]
		"next" => Token[next: Span$.0]
		"throw" => Token[throw: Span$.0]
		"try" => Token[try: Span$.0]
		"catch" => Token[catch: Span$.0]
	)
	my attrs is static is readonly = #(
		"static" => Token[static: Span$.0]
		"hidden" => Token[hidden: Span$.0]
		"readonly" => Token[readonly: Span$.0]
		"friend" => Token[friend: Span$.0]
		"unordered" => Token[unordered: Span$.0]
		"getter" => Token[getter: Span$.0]
		"setter" => Token[setter: Span$.0]
		"main" => Token[main: Span$.0]
		"inline" => Token[inline: Span$.0]
		"noinherit" => Token[noinherit: Span$.0]
		"pattern" => Token[pattern: Span$.0]
		"asm" => Token[asm: Span$.0]
		"native" => Token[native: Span$.0]
		"flags" => Token[flags: Span$.0]
		"uncounted" => Token[uncounted: Span$.0]
		"strong" => Token[strong: Span$.0]
		"sealed" => Token[sealed: Span$.0]
		"macro" => Token[macro: Span$.0]
	)

	my source (SourceFile)
	my reader (Reader)
	my begin = Pos[new]
	
	init [new: source (SourceFile)] {
		this.source = source
		reader = Reader[new: source.text]
	}

	on [tokenize] (Tuple[Tokens, Array[Error]]) {
		my errors = Array[Error] #[]
		my tokens = Tokens[new]

		do {
			try {
				while reader[hasNext] {
					tokens[add: this[readToken]]
				}
			} catch {
				at (Eof) => break
				at my error (Error) {
					errors[add: error]
					next
				}
			}
		}

		tokens[Lexer retoken]

		return #{tokens, errors}
	}

	on [here] (Pos) is inline => return reader.cursor[pos]

	on [span] (Span) is inline => return Span[:begin end: this[here] :source]

	on [beginSpan] (Span) is inline => return Span[at: begin :source]

	on [hereSpan] (Span) is inline => return Span[at: this[here] :source]

	on [trim] {
		while true {
			case {
				at reader[eat: hspace]? => next
				at reader[eat: #";"] && reader[peekNot: vspace] => this[readComment]
				else => break
			}
		}
	}

	on [make: token (Token)] (Token) is inline {
		return token
		-> span = this[span]
	}

	on [readComment] is inline {
		if reader[eat: #"["] {
			this[readNestedComment]
			
			if reader[peek: vspace] {
				reader[next]
			}
		} else {
			while reader[peekNot: vspace] {
				reader[next]
			}
		}
	}

	on [readNestedComment] {
		while true {
			case {
				at !reader[hasNext] => throw LexError[unterminatedComment: this[beginSpan]] ; not entirely accurate but eh
				at reader[eat: #"["] => this[readNestedComment]
				at reader[eat: #"]"] => break
				else => reader[next]
			}
		}
	}

	on [readToken] (Token) {
		my oldBegin = begin = this[here]

		this[trim]

		if !reader[hasNext] => throw Eof[new]

		begin = this[here]

		return this[make: {
			match reader[first] {
				at #"\n" <= _ <= #"\r" {
					begin = oldBegin
					return this[readLSep]
				}

				at #"," {
					reader[next]
					return this[readComma]
				}

				at #"0" <= _ <= #"9" => return this[readNumberStart]
				
				at #"a" <= _ <= #"z" => return this[readName]
				
				at #"_" {
					if reader[at: 1 peek: alnum_q | #":"] {
						return this[readName]
					} else {
						reader[next]
						return Token[wildcard]
					}
				}
				
				at #":" {
					reader[next]
					return this[readPunned]
				}
				
				at #"A" <= _ <= #"Z" => return this[readTypeName]
				
				at #"." {
					if reader[next][eat: #"."] {
						if reader[eat: #"."] {
							return Token[dotDotDot]
						} else {
							throw LexError[invalidOperator: "..", this[span]]
						}
					} else {
						return Token[dot]
					}
				}

				at #"(" {reader[next], return Token[lParen]}
				at #")" {reader[next], return Token[rParen]}
				at #"[" {reader[next], return Token[lBracket]}
				at #"]" {reader[next], return Token[rBracket]}
				at #"{" {reader[next], return Token[lBrace]}
				at #"}" {reader[next], return Token[rBrace]}
				at #"~" {reader[next], return Token[tilde]}
				
				at #"\"" => return this[readStr]
				
				at #"#" => match reader[next][first] {
					at #"a" <= _ <= #"z" => return this[readTag]
					at #"(" {reader[next], return Token[hashLParen]}
					at #"[" {reader[next], return Token[hashLBracket]}
					at #"{" {reader[next], return Token[hashLBrace]}
					at my char => throw LexError[invalidInput: char, this[hereSpan] afterHash: this[beginSpan]]
				}

				;-- =, =>
				at #"=" {
					reader[next]
					case {
						at reader[eat: #">"] => return Token[eqGt]
						at reader[eat: #"="] => throw LexError[invalidEqEq: this[span]]
						else => return Token[eq]
					}
				}

				;-- ?, ?=
				at #"?" {
					if reader[next][eat: #"="] {
						return Token[questionEq]
					} else {
						return Token[question]
					}
				}

				;-- !, !=, !!, !!=
				at #"!" {
					case {
						at reader[next][eat: #"="] => return Token[bangEq]
						at reader[eat: #"!"] {
							if reader[eat: #"="] {
								return Token[bangBangEq]
							} else {
								return Token[bangBang]
							}
						}
						else => return Token[bang]
					}
				}

				;-- +, +=, ++
				at #"+" {
					case {
						at reader[next][eat: #"="] => return Token[plusEq]
						at reader[eat: #"+"] => return Token[plusPlus]
						else => return Token[plus]
					}
				}

				;-- -, -=, --, ->
				at #"-" {
					reader[next]
					case {
						at reader[eat: #"="] => return Token[minusEq]
						at reader[eat: #">"] => return Token[cascade: 1]
						at reader[eat: #"-"] => case {
							at reader[peek: #"-"] {
								my depth = 2

								while reader[eat: #"-"] {
									depth++
								}

								if reader[eat: #">"] {
									return Token[cascade: depth]
								} else {
									throw LexError[unterminatedCascade: this[span], this[hereSpan]]
								}
							}
							at reader[eat: #">"] => return Token[cascade: 2]
							else => return Token[minusMinus]
						}
						else => return Token[minus]
					}
				}

				;-- *, *=, **, **=
				at #"*" {
					case {
						at reader[next][eat: #"="] => return Token[starEq]
						at reader[eat: #"*"] {
							if reader[eat: #"="] {
								return Token[starStarEq]
							} else {
								return Token[starStar]
							}
						}
						else => return Token[star]
					}
				}

				;-- /, /=, //, //=
				at #"/" {
					case {
						at reader[next][eat: #"="] => return Token[divEq]
						at reader[eat: #"/"] {
							if reader[eat: #"="] {
								return Token[divDivEq]
							} else {
								return Token[divDiv]
							}
						}
						else => return Token[div]
					}
				}

				;-- %, %=, %%, %%=
				at #"%" {
					case {
						at reader[next][eat: #"="] => return Token[modEq]
						at reader[eat: #"%"] {
							if reader[eat: #"="] {
								return Token[modModEq]
							} else {
								return Token[modMod]
							}
						}
						else => return Token[mod]
					}
				}

				;-- &, &=, &&, &&=
				at #"&" {
					case {
						at reader[next][eat: #"="] => return Token[andEq]
						at reader[eat: #"&"] {
							if reader[eat: #"="] {
								return Token[andAndEq]
							} else {
								return Token[andAnd]
							}
						}
						else => return Token[and]
					}
				}

				;-- |, |=, ||, ||=
				at #"|" {
					case {
						at reader[next][eat: #"="] => return Token[barEq]
						at reader[eat: #"|"] {
							if reader[eat: #"="] {
								return Token[barBarEq]
							} else {
								return Token[barBar]
							}
						}
						else => return Token[bar]
					}
				}

				;-- ^, ^=, ^^, ^^=
				at #"^" {
					case {
						at reader[next][eat: #"="] => return Token[caretEq]
						at reader[eat: #"^"] {
							if reader[eat: #"="] {
								return Token[caretCaretEq]
							} else {
								return Token[caretCaret]
							}
						}
						else => return Token[caret]
					}
				}

				;-- <, <=, <<, <<=
				at #"<" {
					case {
						at reader[next][eat: #"="] => return Token[ltEq]
						at reader[eat: #"<"] {
							if reader[eat: #"="] {
								return Token[ltLtEq]
							} else {
								return Token[ltLt]
							}
						}
						else => return Token[lt]
					}
				}

				;-- >, >=, >>, >>=
				at #">" {
					case {
						at reader[next][eat: #"="] => return Token[gtEq]
						at reader[eat: #">"] {
							if reader[eat: #"="] {
								return Token[gtGtEq]
							} else {
								return Token[gtGt]
							}
						}
						else => return Token[gt]
					}
				}

				at #"`" {
					reader[next]
					return this[readLitsym]
				}

				at #"$" {
					reader[next]
					return this[readAnonArg]
				}

				else => throw LexError[invalidInput: this[beginSpan]]
			}
		}]
	}

	on [readLSep] (Token) is inline {
		do {
			reader[next]
			this[trim]
		} while reader? && #"\n" <= reader[first] <= #"\r"

		if reader[eat: #","] {
			return this[readCSep]
		} else {
			return Token[lSep]
		}
	}

	on [readCSep] (Token) {
		this[trim]

		while reader? && #"\n" <= reader[first] <= #"\r" {
			reader[next]
			this[trim]
		}

		return Token[cSep]
	}

	on [readComma] (Token) {
		this[trim]

		if reader? && #"\n" <= reader[first] <= #"\r" {
			reader[next]
			return this[readCSep]
		} else {
			return Token[comma]
		}
	}

	on [readNumberStart] (Token) is inline {
		if reader[eat: "0x"] {
			if reader[peek: xdigit] {
				return this[readHex]
			} else {
				throw LexError[invalidHexStart: this[span]]
			}
		} else {
			return this[readNumber]
		}
	}

	on [readHex] (Token) is inline {
		my hex = ""

		while true {
			match reader[eat: xdigit] at Maybe[the: my xdigit'] {
				hex[add: xdigit']
			} else {
				break
			}
		}

		if reader[peek: alpha_u] {
			my end = this[here]

			while reader[eat: alnum_q]? {}

			my endName = this[here]

			throw LexError[
				name: Span[begin: end end: endName :source]
				afterHex: Span[:begin :end :source]
			]
		} else {
			return Token[:hex]
		}
	}

	on [readNumber] (Token) is inline {
		my int = ""

		while true {
			match reader[eat: digit] at Maybe[the: my digit'] {
				int[add: digit']
			} else {
				break
			}
		}

		my afterInt = this[here]

		my dec = {
			if reader[peek: #"."] && reader[at: 1 peekNot: lower_u] {
				if reader[next][peek: digit] {
					my dec' = ""

					while true {
						match reader[eat: digit] at Maybe[the: my digit'] {
							dec'[add: digit']
						} else {
							break
						}
					}

					return Maybe[the: dec']
				} else {
					throw LexError[
						incompleteDecimalPoint:
							Span[at: afterInt :source],
							Span[:begin end: this[here][advance: -1] :source]
					]
				}
			} else {
				return Maybe[none]
			}
		}

		my exp = {
			if reader[eat: #"e"] {
				return Maybe[the: this[readExponent]]
			} else {
				return Maybe[none]
			}
		}

		if reader[peek: alpha_u] {
			my end = this[here]

			while reader[eat: alnum_q]? {}

			my endName = this[here]

			throw LexError[
				name: Span[begin: end end: endName :source]
				afterNumber: Span[:begin :end :source]
			]
		} else {
			match dec at Maybe[the: my dec'] {
				return Token[:int dec: dec' :exp]
			} else {
				return Token[:int :exp]
			}
		}
	}

	on [readExponent] (Str) is inline {
		my exp = ""
		my begin' = this[here]

		match reader[first] at my sign = #"+" || #"-" {
			reader[next]
			exp[add: sign]
		}

		if reader[peek: digit] {
			while true {
				match reader[eat: digit] at Maybe[the: my digit'] {
					exp[add: digit']
				} else {
					break
				}
			}

			return exp
		} else {
			my end = this[here]

			throw LexError[
				missingExponent:
					Span[begin: begin'[advance: -1] :end :source],
					Span[begin: end end: end[advance] :source]
			]
		}
	}

	on [readName] (Token) is inline {
		my name = ""

		while true {
			match reader[eat: alnum_q] at Maybe[the: my char] {
				name[add: char]
			} else {
				break
			}
		}

		if reader[eat: #":"] {
			return Token[label: name]
		} else {
			return Token[:name]
		}
	}

	on [readPunned] (Token) is inline {
		my punned = ""

		match reader[eat: lower_u] at Maybe[the: my char] {
			punned[add: char]
		} else {
			my end = this[here]

			if reader[peek: upper] {
				while reader[eat: alnum_q]? {}

				my endName = this[here]

				throw LexError[
					noUppercasePunnedLabel:
						this[beginSpan],
						Span[at: end :source],
						Span[begin: end end: endName :source]
				]
			} else {
				throw LexError[
					incompletePunnedLabel:
						this[beginSpan],
						Span[at: begin[advance] :source]
				]
			}
		}

		while true {
			match reader[eat: alnum_q] at Maybe[the: my char] {
				punned[add: char]
			} else {
				break
			}
		}

		return Token[:punned]
	}

	on [readTypeName] (Token) is inline {
		my name = ""

		while true {
			match reader[eat: alnum_q] at Maybe[the: my char] {
				name[add: char]
			} else {
				break
			}
		}

		if reader[peek: #":"] {
			throw LexError[
				noUppercaseLabel:
					this[beginSpan],
					Span[begin: begin[advance] end: this[here] :source]
			]
		} else {
			return Token[typeName: name]
		}
	}

	on [readLitsym] (Token) is inline {
		my sym = ""

		while true {
			my char = reader[first]

			reader[next]

			if char ?= #"`" {
				break
			} else {
				sym[add: char]
			}
		}

		return Token[litsym: sym]
	}

	on [readTag] (Token) is inline {
		my tag = ""

		while true {
			match reader[eat: alnum] at Maybe[the: my char] {
				tag[add: char]
			}
		}

		return Token[:tag]
	}

	on [readChar] (Token) is inline {
		my char = {
			match reader[eat] {
				at #"\"" {
					my end = this[here][advance: -1]

					if reader[eat: #"\""] {
						throw LexError[
							escapeCharQuote:
								Span[:begin :end :source],
								Span[at: end :source],
								this[hereSpan]
						]
					} else {
						throw LexError[noEmptyChar: this[span]]
					}
				}

				at #"\\" => match reader[eat] {
					at my c = #"\\" || #"\"" => return c
					at #"t" => return #"\t"
					at #"n" => return #"\n"
					at #"r" => return #"\r"
					at #"v" => return #"\v"
					at #"f" => return #"\f"
					at #"0" => return #"\0"
					at #"e" => return #"\e"
					at #"a" => return #"\a"
					at #"b" => return #"\b"
					at #"x" => return this[readHexEsc]
					at #"u" => return this[readUniEsc]
					at #"o" => return this[readOctEsc]
					at my c {
						my end = this[here][advance: -1]
						throw LexError[
							invalidCharEscape:
								Span[:begin :end :source],
								c,
								Span[at: end :source],
								Span[at: end[advance] :source]
						]
					}
				}
				
				at my char' => return char'
			}
		}

		if reader[eat: #"\""] {
			return Token[:char]
		} else {
			throw LexError[unterminatedChar: this[span], this[hereSpan]]
		}
	}

	on [readHexEsc] (Char) {
		my hex = ""

		for _ from: 1 times: 2 {
			match reader[eat: xdigit] at Maybe[the: my xdigit'] {
				hex[add: xdigit']
			} else {
				my end = this[here]
				throw LexError[
					invalidHexEscape:
						Span[begin: end[advance: -(hex.length + 2)] :end :source],
						this[hereSpan]
				]
			}
		}

		return Int[Power fromStr: hex withBase: 16 includesPrefix: false][Char]
	}
	
	on [readUniEsc] (Char) {
		my uni = ""

		for _ from: 1 times: 4 {
			match reader[eat: xdigit] at Maybe[the: my xdigit'] {
				uni[add: xdigit']
			} else {
				my end = this[here]
				throw LexError[
					invalidUniEscape:
						Span[begin: end[advance: -(uni.length + 2)] :end :source],
						this[hereSpan]
				]
			}
		}

		;@@ TODO: determine if `Char` should be unicode here
		return Int[Power fromStr: uni withBase: 16 includesPrefix: false][Char]
	}

	on [readOctEsc] (Char) {
		my oct = ""

		for _ from: 1 times: 3 {
			match reader[eat: odigit] at Maybe[the: my odigit'] {
				oct[add: odigit']
			} else {
				my end = this[here]
				throw LexError[
					invalidOctEscape:
						Span[begin: end[advance: -(oct.length + 2)] :end :source],
						this[hereSpan]
				]
			}
		}

		return Int[Power fromStr: oct withBase: 8 includesPrefix: false][Char]
	}

	on [readStr] (Token) is inline {
		my seg = ""
		my segments = Array[StrSegment] #[]

		while reader[hasNext] {
			match reader[eat] {
				at #"\"" {
					if seg? {
						segments[add: StrSegment[str: seg]]
					}

					break
				}

				at #"\\" {
					if seg? {
						segments[add: StrSegment[str: seg]]
						seg = ""
					}

					my esc = reader[eat]
					
					segments[add: {
						if esc ?= #"(" {
							return StrSegment[code: {
								this[trim]
								
								my level = 1
								my tokens' = Tokens[new]

								while level > 0 {
									tokens'[add: this[readToken] -> {
										match this {
											at Token[lParen] || Token[hashLParen] => level++
											at Token[rParen] {
												if --level ?= 0 {
													break
												}
											}
											else {}
										}
									}]

									this[trim]
								}

								return tokens'
							}]
						} else {
							return StrSegment[char: {
								match esc {
									at my c = #"\\" || #"\"" => return c
									at #"t" => return #"\t"
									at #"n" => return #"\n"
									at #"r" => return #"\r"
									at #"v" => return #"\v"
									at #"f" => return #"\f"
									at #"0" => return #"\0"
									at #"e" => return #"\e"
									at #"a" => return #"\a"
									at #"b" => return #"\b"
									at #"x" => return this[readHexEsc]
									at #"u" => return this[readUniEsc]
									at #"o" => return this[readOctEsc]
									at my c {
										my end = this[here]
										throw LexError[
											invalidStrEscape:
												c,
												Span[begin: end[advance: -2] :end :source] ;@@ off by 1 error?
										]
									}
								}
							}]
						}
					}]
				}
				
				at my char => seg[add: char]
			}
		}

		if reader[eat: #"\""] {
			return Token[str: segments]
		} else {
			throw LexError[unterminatedStr: this[beginSpan]]
		}
	}

	on [readAnonArg] (Token) is inline {
		my depth = 0

		while reader[eat: #"."] {
			depth++
		}

		if reader[peek: digit] {
			my nth = ""

			do {
				nth[add: reader[eat]]
			} while reader[peek: digit]

			if reader[peek: alpha_u] {
				my end = this[here]

				while reader[eat: alnum_q]? {}

				my endName = this[here]

				throw LexError[
					name: Span[begin: end end: endName :source]
					afterAnonArg: Span[:begin :end :source]
				]
			} else {
				return Token[anonArg: nth[Int] :depth]
			}
		} else {
			throw LexError[unterminatedAnonArg: this[span], this[hereSpan]]
		}
	}
}

category Lexer for Tokens is hidden {
	on [retoken] {
		match this {
			at #[
				(
					|| Token[lParen]
					|| Token[lBracket]
					|| Token[lBrace]
					|| Token[hashLParen]
					|| Token[hashLBracket]
					|| Token[hashLBrace]
				)
				Token[lSep]
				...my rest
			] {
				this[removeAt: 1]
				rest[Lexer retoken]
			}

			at #[
				Token[lSep]
				Token[rParen] || Token[rBracket] || Token[rBrace]
				...my rest
			] {
				this[removeAt: 0]
				rest[Lexer retoken]
			}

			at #[Token[dot], Token[name: _], ...my rest] => rest[Lexer retoken]
			
			at #[Token[name: "this" span: my span], ...my rest] {
				this[at: 0] = Token[this: span]
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
				this[at: 0] = Token[my: span]
				rest[Lexer retoken]
			}
			at #[Token[name: "has" span: my span], Token[name: _], ...my rest] {
				this[at: 0] = Token[has: span]
				rest[Lexer retoken]
			}

			at #[Token[name: "is" span: my span], Token[name: my name span: my span'], ...my rest] {
				this[at: 0] = Token[is: span]
				
				match Lexer.attrs[maybeAt: name] at Maybe[the: my tokenFunc] {
					this[at: 1] = tokenFunc[call: span']
				}

				rest[Lexer retoken]
			}
			at #[Token[name: my name span: my span], ...my rest] if Lexer.keywords[containsKey: name] {
				this[at: 0] = Lexer.keywords[at: name][call: span]
				
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

			at #[Token[lSep]] => this[remove]
			at #[_, ...my rest] => rest[Lexer retoken]
			at #[] {}
		}
	}
}