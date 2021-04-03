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

	my input (SourceFile)
	my reader (Reader)
	my begin = Pos[new]
	my tokens = Tokens[new]
	
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
		return reader.cursor[pos]
	}

	on [span] (Span) is inline {
		return Span[:begin end: this[here] :source]
	}

	on [trim] {
		while true {
			if reader[eat: hspace]? {
				next
			} orif reader[eat: #";"] && reader[peekNot: vspace] {
				this[readComment]
			} else {
				break
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
				at !reader[hasNext] => throw "Unterminated comment!"
				at reader[eat: #"["] => this[readNestedComment]
				at reader[eat: #"]"] => break
				else => reader[next]
			}
		}
	}

	on [readToken] (Token) {
		begin = this[here]

		this[trim]

		if reader[eat: vspace] {
			return this[make: this[readLSep]]
		}

		begin[skip: reader]

		return this[make: {
			match reader[first] {
				at #"," {
					reader[next]
					return this[readComma]
				}

				at #"0" <= _ <= #"9" => return this[readNumberStart]
				
				at #"a" <= _ <= #"z" => return this[readName]
				
				at #"_" {
					if reader[at: 1 peek: alnum_q] {
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
							throw Diagnostic[
								severity: Severity.error
								message: "Syntax error",
								info: #[
									Diagnostic.Info[
										span: this[span]
										message: "Invalid operator `..`"
										priority: Diagnostic.Info.Priority.primary
									]
								]
							]
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
					at my char => throw Diagnostic[
						severity: Severity.error
						message: "Syntax error"
						info: #[
							Diagnostic.Info[
								span: Span[at: this[here] :source]
								message: "Unexpected `\(char)` after `#`"
								priority: Diagnostic.Info.Priority.primary
							]
							Diagnostic.Info[
								span: Span[at: begin :source]
								priority: Diagnostic.Info.Priority.secondary
							]
						]
					]
				}

				;-- =, =>
				at #"=" {
					if reader[next][eat: #">"] {
						return Token[eqGt]
					} orif reader[eat: #"="] {
						throw Diagnostic[
							severity: Severity.error
							message: "Syntax error"
							info: #[
								Diagnostic.Info[
									span: this[span]
									message: "Please use `?=` instead of `==` in Star"
									priority: Diagnostic.Info.Priority.primary
								]
							]
						]
					} else {
						return Token[eq]
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
					if reader[next][eat: #"="] {
						return Token[bangEq]
					} orif reader[eat: #"!"] {
						if reader[eat: #"="] {
							return Token[bangBangEq]
						} else {
							return Token[bangBang]
						}
					} else {
						return Token[bang]
					}
				}

				;-- +, +=, ++
				at #"+" {
					if reader[next][eat: #"="] {
						return Token[plusEq]
					} orif reader[eat: #"+"] {
						return Token[plusPlus]
					} else {
						return Token[plus]
					}
				}

				;-- -, -=, --, ->
				at #"-" {
					reader[next]
					case {
						at reader[eat: #"="] => return Token[minusEq]
						at reader[eat: #">"] => return Token[cascade: 1]
						at reader[eat: #"-"] {
							if reader[peek: #"-"] {
								my depth = 2

								while reader[eat: #"-"] {
									depth++
								}

								if reader[eat: #">"] {
									return Token[cascade: depth]
								} else {
									throw Diagnostic[
										severity: Severity.error
										message: "Unterminated cascade"
										info: #[
											Diagnostic.Info[
												span: Span[at: this[here] :source]
												message: "Expected a `>` to finish the cascade operator"
												priority: Diagnostic.Info.Priority.primary
											]
											Diagnostic.Info[
												span: this[span]
												priority: Diagnostic.Info.Priority.secondary
											]
										]
									]
								}
							} orif reader[eat: #">"] {
								return Token[cascade: 2]
							} else {
								return Token[minusMinus]
							}
						}
						else => return Token[minus]
					}
				}

				;-- *, *=, **, **=
				at #"*" {
					if reader[next][eat: #"="] {
						return Token[starEq]
					} orif reader[eat: #"*"] {
						if reader[eat: #"="] {
							return Token[starStarEq]
						} else {
							return Token[starStar]
						}
					} else {
						return Token[star]
					}
				}

				;-- /, /=, //, //=
				at #"/" {
					if reader[next][eat: #"="] {
						return Token[divEq]
					} orif reader[eat: #"/"] {
						if reader[eat: #"="] {
							return Token[divDivEq]
						} else {
							return Token[divDiv]
						}
					} else {
						return Token[div]
					}
				}

				;-- %, %=, %%, %%=
				at #"%" {
					if reader[next][eat: #"="] {
						return Token[modEq]
					} orif reader[eat: #"%"] {
						if reader[eat: #"="] {
							return Token[modModEq]
						} else {
							return Token[modMod]
						}
					} else {
						return Token[mod]
					}
				}

				;-- &, &=, &&, &&=
				at #"&" {
					if reader[next][eat: #"="] {
						return Token[andEq]
					} orif reader[eat: #"&"] {
						if reader[eat: #"="] {
							return Token[andAndEq]
						} else {
							return Token[andAnd]
						}
					} else {
						return Token[and]
					}
				}

				;-- |, |=, ||, ||=
				at #"|" {
					if reader[next][eat: #"="] {
						return Token[barEq]
					} orif reader[eat: #"|"] {
						if reader[eat: #"="] {
							return Token[barBarEq]
						} else {
							return Token[barBar]
						}
					} else {
						return Token[bar]
					}
				}

				;-- ^, ^=, ^^, ^^=
				at #"^" {
					if reader[next][eat: #"="] {
						return Token[caretEq]
					} orif reader[eat: #"^"] {
						if reader[eat: #"="] {
							return Token[caretCaretEq]
						} else {
							return Token[caretCaret]
						}
					} else {
						return Token[caret]
					}
				}

				;-- <, <=, <<, <<=
				at #"<" {
					if reader[next][eat: #"="] {
						return Token[ltEq]
					} orif reader[eat: #"<"] {
						if reader[eat: #"="] {
							return Token[ltLtEq]
						} else {
							return Token[ltLt]
						}
					} else {
						return Token[lt]
					}
				}

				;-- >, >=, >>, >>=
				at #">" {
					if reader[next][eat: #"="] {
						return Token[gtEq]
					} orif reader[eat: #">"] {
						if reader[eat: #"="] {
							return Token[gtGtEq]
						} else {
							return Token[gtGt]
						}
					} else {
						return Token[gt]
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

				else => throw Diagnostic[
					severity: Severity.error
					message: "Syntax error"
					info: #[
						Diagnostic.Info[
							span: Span[at: begin :source]
							message: "This is not the syntax that you are looking for"
							priority: Diagnostic.Info.Priority.primary
						]
					]
				]
			}
		}]
	}

	on [readLSep] (Token) is inline {
		do {
			this[trim]
		} while reader[eat: vspace]?

		if reader[eat: #","] {
			return this[readCSep]
		} else {
			return Token[lSep]
		}
	}

	on [readCSep] (Token) {
		do {
			this[trim]
		} while reader[eat: vspace]?

		return Token[cSep]
	}

	on [readComma] (Token) {
		this[trim]

		if reader[eat: vspace]? {
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
				throw Diagnostic[
					severity: Severity.error
					message: "Unexpected start of hexdecimal literal"
					info: #[
						Diagnostic.Info[
							span: this[span]
							message: "Were you wanting a hexdecimal literal here or what?"
							priority: Diagnostic.Info.Priority.primary
						]
					]
				]
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

			throw Diagnostic[
				severity: Severity.error
				message: "Invalid hexdecimal literal"
				info: #[
					Diagnostic.Info[
						span: Span[begin: end end: endName :source]
						message: "Make sure to separate names from numbers"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: Span[:begin :end :source]
						priority: Diagnostic.Info.Priority.secondary
					]
				]
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
					throw Diagnostic[
						severity: Severity.error
						message: "Invalid decimal literal"
						info: #[
							Diagnostic.Info[
								span: Span[at: afterInt :source]
								message: "At least 1 digit is required on both sides of the decimal point"
								priority: Diagnostic.Info.Priority.primary
							]
							Diagnostic.Info[
								span: Span[:begin end: this[here][advance: -2] :source]
								priority: Diagnostic.Info.Priority.secondary
							]
						]
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

			throw Diagnostic[
				severity: Severity.error
				message: "Invalid number literal"
				info: #[
					Diagnostic.Info[
						span: Span[begin: end end: endName :source]
						message: "Make sure to separate names from numbers"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: Span[:begin :end :source]
						priority: Diagnostic.Info.Priority.secondary
					]
				]
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

			throw Diagnostic[
				severity: Severity.error
				message: "Invalid number literal"
				info: #[
					Diagnostic.Info[
						span: Span[begin: end end: end[advance] :source]
						message: "Make sure to separate names from numbers"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: Span[begin: begin'[advance: -1] :end :source]
						message: "This indicates that the number has an exponent"
						priority: Diagnostic.Info.Priority.secondary
					]
				]
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

			throw Diagnostic[
				severity: Severity.error
				message: "Invalid punned label"
				info: {
					if reader[peek: upper] {
						while reader[eat: alnum_q]? {}

						my endName = this[here]

						return #[
							Diagnostic.Info[
								span: Span[at: end :source]
								message: "Punned labels may not start with an uppercase letter"
								priority: Diagnostic.Info.Priority.primary
							]
							Diagnostic.Info[
								span: Span[at: begin :source]
								priority: Diagnostic.Info.Priority.secondary
							]
							Diagnostic.Info[
								span: Span[begin: end end: endName :source]
								priority: Diagnostic.Info.Priority.secondary
							]
						]
					} else {
						return #[
							Diagnostic.Info[
								span: Span[at: end :source]
								message: "Was expecting a name for the punned label"
								priority: Diagnostic.Info.Priority.primary
							]
							Diagnostic.Info[
								span: Span[at: begin :source]
								priority: Diagnostic.Info.Priority.secondary
							]
						]
					}
				}
			]
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
			my end = this[here]

			throw Diagnostic[
				severity: Severity.error
				message: "Invalid label"
				info: #[
					Diagnostic.Info[
						span: Span[at: begin :source]
						message: "Labels may not start with an uppercase letter"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: Span[begin: begin[advance] end: end[advance] :source]
						priority: Diagnostic.Info.Priority.secondary
					]
				]
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
					my end = this[here]

					if reader[peek: #"\""] {
						throw Diagnostic[
							severity: Severity.error
							message: "Invalid char literal"
							info: #[
								Diagnostic.Info[
									span: Span[at: end :source]
									message: "`\"` characters need to be escaped in char literals"
									priority: Diagnostic.Info.Priority.primary
								]
								Diagnostic.Info[
									span: this[span]
									priority: Diagnostic.Info.Priority.secondary
								]
								Diagnostic.Info[
									span: Span[at: end[advance] :source]
									priority: Diagnostic.Info.Priority.secondary
								]
							]
						]
					} else {
						throw Diagnostic[
							severity: Severity.error
							message: "Invalid char literal"
							info: #[
								Diagnostic.Info[
									span: Span[:begin end: end[advance] :source]
									message: "Char literals may not be empty"
									priority: Diagnostic.Info.Priority.primary
								]
							]
						]
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
						throw Diagnostic[
							severity: Severity.error
							message: "Invalid escape character"
							info: #[
								Diagnostic.Info[
									span: Span[at: end :source]
									message: "Escape character `\(c)` \({
										if c ?= #"(" {
											return "is not allowed in char literals"
										} else {
											return "does not exist"
										}
									})"
									priority: Diagnostic.Info.Priority.primary
								]
								Diagnostic.Info[
									span: this[span]
									priority: Diagnostic.Info.Priority.secondary
								]
								Diagnostic.Info[
									span: Span[at: end[advance] :source]
									priority: Diagnostic.Info.Priority.secondary
								]
							]
						]
					}
				}
				
				at my char' => return char'
			}
		}

		if reader[eat: #"\""] {
			return Token[:char]
		} else {
			throw Diagnostic[
				severity: Severity.error
				message: "Unterminated character literal"
				info: #[
					Diagnostic.Info[
						span: Span[at: this[here] :source]
						message: "Expected another `\"` to finish the char literal"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: this[span]
						priority: Diagnostic.Info.Priority.secondary
					]
				]
			]
		}
	}

	on [readHexEsc] (Char) {
		my hex = ""

		for _ from: 1 to: 2 {
			match reader[eat: xdigit] at Maybe[the: my xdigit'] {
				hex[add: xdigit']
			} else {
				my end = this[here]
				throw Diagnostic[
					severity: Severity.error
					message: "Invalid hexdecimal escape code"
					info: #[
						Diagnostic.Info[
							span: Span[at: end :source]
							message: "Was expecting a hexdecimal digit here"
							priority: Diagnostic.Info.Priority.primary
						]
						Diagnostic.Info[
							span: Span[begin: end[advance: -(hex.length + 2)] :end :source]
							priority: Diagnostic.Info.Priority.secondary
						]
					]
				]
			}
		}

		return Int[Power fromStr: hex withBase: 16 includesPrefix: false][Char]
	}
	
	on [readUniEsc] (Char) {
		my uni = ""

		for _ from: 1 to: 4 {
			match reader[eat: xdigit] at Maybe[the: my xdigit'] {
				uni[add: xdigit']
			} else {
				my end = this[here]
				throw Diagnostic[
					severity: Severity.error
					message: "Invalid unicode escape code"
					info: #[
						Diagnostic.Info[
							span: Span[at: end :source]
							message: "Was expecting a hexdecimal digit here"
							priority: Diagnostic.Info.Priority.primary
						]
						Diagnostic.Info[
							span: Span[begin: end[advance: -(uni.length + 2)] :end :source]
							priority: Diagnostic.Info.Priority.secondary
						]
					]
				]
			}
		}

		;@@ TODO: determine if `Char` should be unicode here
		return Int[Power fromStr: uni withBase: 16 includesPrefix: false][Char]
	}

	on [readOctEsc] (Char) {
		my oct = ""

		for _ from: 1 to: 3 {
			match reader[eat: odigit] at Maybe[the: my odigit'] {
				oct[add: odigit']
			} else {
				my end = this[here]
				throw Diagnostic[
					severity: Severity.error
					message: "Invalid octal escape code"
					info: #[
						Diagnostic.Info[
							span: Span[at: end :source]
							message: "Was expecting an octal digit here"
							priority: Diagnostic.Info.Priority.primary
						]
						Diagnostic.Info[
							span: Span[begin: end[advance: -(oct.length + 2)] :end :source]
							priority: Diagnostic.Info.Priority.secondary
						]
					]
				]
			}
		}

		return Int[Power fromStr: oct withBase: 8 includesPrefix: false][Char]
	}

	on [readStr] (Token) is inline {
		my seg = ""
		my segments = #[]

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
										my end = this[here][advance: -1]
										throw Diagnostic[
											severity: Severity.error
											message: "Invalid escape character"
											info: #[
												Diagnostic.Info[
													span: Span[at: end :source]
													message: "Escape character `\\\(c)` does not exist"
													priority: Diagnostic.Info.Priority.primary
												]
												Diagnostic.Info[
													span: Span[at: end[advance: -1] :source]
													priority: Diagnostic.Info.Priority.secondary
												]
											]
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
			throw Diagnostic[
				severity: Severity.error
				message: "Unterminated string"
				info: #[
					Diagnostic.Info[
						span: Span[at: begin :source]
						message: "This string is never terminated"
						priority: Diagnostic.Info.Priority.primary
					]
				]
			]
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

				throw Diagnostic[
					severity: Severity.error
					message: "Invalid anonymous argument"
					info: #[
						Diagnostic.Info[
							span: Span[begin: end end: endName :source]
							message: "Make sure to separate names from numbers"
							priority: Diagnostic.Info.Priority.primary
						]
						Diagnostic.Info[
							span: Span[:begin :end :source]
							priority: Diagnostic.Info.Priority.secondary
						]
					]
				]
			} else {
				return Token[anonArg: nth[Int] :depth]
			}
		} else {
			throw Diagnostic[
				severity: Severity.error
				message: "Unterminated anonymous argument"
				info: #[
					Diagnostic.Info[
						span: Span[at: this[here] :source]
						message: "Was expecting a number here"
						priority: Diagnostic.Info.Priority.primary
					]
					Diagnostic.Info[
						span: this[span]
						priority: Diagnostic.Info.Priority.secondary
					]
				]
			]
		}
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
				this[at: 0] = Token[my]->[:span]
				rest[Lexer retoken]
			}
			at #[Token[name: "has" span: my span], Token[name: _], ...my rest] {
				this[at: 0] = Token[has]->[:span]
				rest[Lexer retoken]
			}

			at #[Token[name: "is" span: my span], Token[name: my name span: my span'], ...my rest] {
				this[at: 0] = Token[is]->[:span]
				
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