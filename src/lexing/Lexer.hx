package lexing;

import text.SourceFile;
import text.Pos;
import hx.strings.Char;
import text.Span;
import lexing.Token;
import reporting.*;
import errors.Error;

using util.Strings;

class Lexer {
	final reader: Reader;
	final source: SourceFile;
	var begin: Pos;

	public inline function new(source: SourceFile) {
		this.source = source;
		begin = new Pos(0, 0);
		reader = new Reader(source.text);
	}

	
	public function tokenize() {
		var errors = Nil;
		var tokens = Nil;
		while(true) {
			try {
				while(reader.hasNext()) {
					tokens = tokens.prepend(readToken());
				}
				break;
			} catch(eof: Eof) {
				break;
			} catch(error: Error) { // this uses Dynamic but whatever
				errors = errors.prepend(error);
			}
		}

		return tuple(
			errors.rev(),
			retoken(Nil, tokens._match(
				at([T_LSep(_), ...rest]) => rest,
				_ => tokens
			))
		);
	}


	static final KEYWORDS = [
		"module" => T_Module,
		"my" => T_My,
		"on" => T_On,
		"return" => T_Return,
		"init" => T_Init,
		"deinit" => T_Deinit,
		"operator" => T_Operator,
		"class" => T_Class,
		"alias" => T_Alias,
		"type" => T_Type,
		"kind" => T_Kind,
		"category" => T_Category,
		"protocol" => T_Protocol,
		"is" => T_Is,
		"of" => T_Of,
		"use" => T_Use,
		"has" => T_Has,
		"if" => T_If,
		"else" => T_Else,
		"while" => T_While,
		"for" => T_For,
		"do" => T_Do,
		"case" => T_Case,
		"match" => T_Match,
		"at" => T_At,
		"break" => T_Break,
		"next" => T_Next,
		"throw" => T_Throw,
		"try" => T_Try,
		"catch" => T_Catch
	];

	static final ATTRS = [
		"static" => T_Static,
		"hidden" => T_Hidden,
		"readonly" => T_Readonly,
		"friend" => T_Friend,
		"unordered" => T_Unordered,
		"getter" => T_Getter,
		"setter" => T_Setter,
		"main" => T_Main,
		"inline" => T_Inline,
		"noinherit" => T_Noinherit,
		"pattern" => T_Pattern,
		"asm" => T_Asm,
		"native" => T_Native,
		"flags" => T_Flags,
		"uncounted" => T_Uncounted,
		"strong" => T_Strong,
		"sealed" => T_Sealed,
		"macro" => T_Macro
	];

	static function retoken(made: Tokens, tokens: Tokens) {
		return tokens._match(
			at([T_LSep(_), l =
				( T_LParen(_)
				| T_LBracket(_)
				| T_LBrace(_)
				| T_HashLParen(_)
				| T_HashLBracket(_)
				| T_HashLBrace(_)
			), ...rest]) => retoken(List.of(l, ...made), rest),

			at([r =
				( T_RParen(_)
				| T_RBracket(_)
				| T_RBrace(_)
			), T_LSep(_), ...rest]) => retoken(List.of(r, ...made), rest),

			at([n = T_Name(_, _), d = T_Dot(_), ...rest]) => retoken(List.of(d, n, ...made), rest),

			at([n = T_Name(_, _), T_Name(span, "my"), ...rest]) => retoken(List.of(T_My(span), n, ...made), rest),
			at([n = T_Name(_, _), T_Name(span, "has"), ...rest]) => retoken(List.of(T_Has(span), n, ...made), rest),
			
			at([T_Name(span, "this"), ...rest]) => retoken(List.of(T_This(span), ...made), rest),
			at([T_Name(span, "true"), ...rest]) => retoken(List.of(T_Bool(span, true), ...made), rest),
			at([T_Name(span, "false"), ...rest]) => retoken(List.of(T_Bool(span, false), ...made), rest),
			
			at([T_Name(span2, ATTRS[_] => attr!), T_Name(span1, "is"), ...rest]) =>
				retoken(List.of(T_Is(span1), attr(span2), ...made), rest),
			
			at([T_Name(span, KEYWORDS[_] => kw!), ...rest]) => retoken(List.of(kw(span), ...made), rest),
			
			at([T_Str(span, segs), ...rest]) => retoken(List.of(T_Str(span, retokenStr(Nil, segs)), ...made), rest),

			at([] | [T_LSep(_)]) => made,
			at([t, ...rest]) => retoken(List.of(t, ...made), rest)
		);
	}

	static function retokenStr(made: List<StrSegment>, segs: List<StrSegment>) {
		return segs._match(
			at([SCode(strTokens), ...rest]) => retokenStr(List.of(SCode(retoken(Nil, strTokens)), ...made), rest),
			at([seg, ...rest]) => retokenStr(List.of(seg, ...made), rest),
			at([]) => made
		);
	}


	inline function here() {
		return reader.cursor.pos();
	}

	inline function span() {
		return new Span(begin, here(), source);
	}

	inline function trim() {
		while(reader.hasNext()) reader.unsafePeek()._match(
			at(' '.code | '\t'.code) => reader.next(),
			at(';'.code) => {
				reader.next();
				if(reader.hasNext()) reader.unsafePeek()._match(
					at('\n'.code ... '\r'.code) => break,
					_ => readComment()
				);
			},
			_ => break
		);
	}

	inline function readComment() {
		if(reader.eat('['.code)) {
			readNestedComment();
			if(reader.hasNext()) reader.unsafePeek()._match(
				at('\n'.code ... '\r'.code) => reader.next(),
				_ => {}
			);
		} else {
			while(reader.hasNext()) reader.unsafePeek()._match(
				at('\n'.code ... '\r'.code) => break,
				_ => reader.next()
			);
		}
	}

	function readNestedComment(): Void {
		while(reader.hasNext()) reader.eat()._match(
			at('['.code) => readNestedComment(),
			at(']'.code) => return,
			_ => {}
		);
		
		throw Lex_UnterminatedComment(Span.at(begin, source)); // not entirely accurate but eh
	}

	/*inline*/ function readToken(): Token {
		final oldBegin = begin = here();

		trim();

		if(!reader.hasNext()) {
			throw new Eof();
		}

		final cur = reader.unsafePeek();
		
		begin = here();

		return cur._match(
			at('\n'.code ... '\r'.code) => {
				begin = oldBegin;
				readLSep();
			},

			at(','.code) => {
				reader.next();
				readComma();
			},

			at('0'.code ... '9'.code) => {
				readNumberStart();
			},

			at('a'.code ... 'z'.code) => {
				readName();
			},

			at('_'.code) => {
				reader.unsafePeekAt(1)._match(
					at(('a'.code ... 'z'.code)
					 | ('A'.code ... 'Z'.code)
					 | ('0'.code ... '9'.code)
					 | '_'.code
					 | "'".code
					 | ':'.code
					) => {
						readName();
					},
					_ => {
						reader.next();
						T_Wildcard(span());
					}
				);
			},

			at(':'.code) => {
				reader.next();
				readPunned();
			},

			at('A'.code ... 'Z'.code) => {
				readTypeName();
			},

			at('.'.code) => {
				reader.next();
				if(reader.eat('.'.code)) {
					if(reader.eat('.'.code)) {
						T_DotDotDot(span());
					} else {
						throw Lex_InvalidOperator("..", span());
					}
				} else {
					T_Dot(span());
				}
			},

			at('('.code) => { reader.next(); T_LParen(span()); },
			at(')'.code) => { reader.next(); T_RParen(span()); },
			at('['.code) => { reader.next(); T_LBracket(span()); },
			at(']'.code) => { reader.next(); T_RBracket(span()); },
			at('{'.code) => { reader.next(); T_LBrace(span()); },
			at('}'.code) => { reader.next(); T_RBrace(span()); },
			at('~'.code) => { reader.next(); T_Tilde(span()); },
			
			at('"'.code) => {
				reader.next();
				if(reader.eat('"'.code))
					T_Str(span(), Nil);
				else
					readStr();
			},

			at('#'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('a'.code ... 'z'.code) => readTag(),
					at('('.code) => { reader.next(); T_HashLParen(span()); },
					at('['.code) => { reader.next(); T_HashLBracket(span()); },
					at('{'.code) => { reader.next(); T_HashLBrace(span()); },
					at('"'.code) => { reader.next(); readChar(); },
					_ => throw Lex_InvalidInputAfterHash(
						Span.at(begin, source),
						reader.peek(),
						Span.at(here(), source)
					)
				);
			},

			// =, =>
			at('='.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('>'.code) => { reader.next(); T_EqGt(span()); },
					at('='.code) => {
						reader.next();
						throw Lex_InvalidEqEq(span());
					},
					_ => T_Eq(span())
				);
			},

			// ?, ?=
			at('?'.code) => {
				reader.next();
				if(reader.eat('='.code))
					T_QuestionEq(span())
				else
					T_Question(span());
			},

			// !, !=, !!, !!=
			at('!'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_BangEq(span()); },
					at('!'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_BangBangEq(span())
						else
							T_BangBang(span());
					},
					_ => T_Bang(span())
				);
			},

			// +, +=, ++
			at('+'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_PlusEq(span()); },
					at('+'.code) => { reader.next(); T_PlusPlus(span()); },
					_ => T_Plus(span())
				);
			},

			// -, -=, --, ->
			at('-'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_MinusEq(span()); },
					at('-'.code) => {
						reader.next();
						reader.unsafePeek()._match(
							at('-'.code) => {
								reader.next();
								
								var depth = 2;

								while(reader.eat('-'.code)) depth++;

								if(reader.eat('>'.code)) {
									T_Cascade(span(), depth);
								} else {
									final end = here();
									throw Lex_UnterminatedCascade(
										new Span(begin, end, source),
										Span.at(end, source)
									);
								}
							},
							at('>'.code) => { reader.next(); T_Cascade(span(), 2); },
							_ => T_MinusMinus(span())
						);
					},
					at('>'.code) => { reader.next(); T_Cascade(span(), 1); },
					_ => T_Minus(span())
				);
			},

			// *, *=, **, **=
			at('*'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_StarEq(span()); },
					at('*'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_StarStarEq(span())
						else
							T_StarStar(span());
					},
					_ => T_Star(span())
				);
			},

			// /, /=, //, //=
			at('/'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_DivEq(span()); },
					at('/'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_DivDivEq(span())
						else
							T_DivDiv(span());
					},
					_ => T_Div(span())
				);
			},

			// %, %=, %%, %%=
			at('%'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_ModEq(span()); },
					at('%'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_ModModEq(span())
						else
							T_ModMod(span());
					},
					_ => T_Mod(span())
				);
			},

			// &, &=, &&, &&=
			at('&'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_AndEq(span()); },
					at('&'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_AndAndEq(span())
						else
							T_AndAnd(span());
					},
					_ => T_And(span())
				);
			},

			// |, |=, ||, ||=
			at('|'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_BarEq(span()); },
					at('|'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_BarBarEq(span())
						else
							T_BarBar(span());
					},
					_ => T_Bar(span())
				);
			},

			// ^, ^=, ^^, ^^=
			at('^'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_CaretEq(span()); },
					at('^'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_CaretCaretEq(span())
						else
							T_CaretCaret(span());
					},
					_ => T_Caret(span())
				);
			},

			// <, <=, <<, <<=
			at('<'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_LtEq(span()); },
					at('<'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_LtLtEq(span())
						else
							T_LtLt(span());
					},
					_ => T_Lt(span())
				);
			},

			// >, >=, >>, >>=
			at('>'.code) => {
				reader.next();
				reader.unsafePeek()._match(
					at('='.code) => { reader.next(); T_GtEq(span()); },
					at('>'.code) => {
						reader.next();
						if(reader.eat('='.code))
							T_GtGtEq(span())
						else
							T_GtGt(span());
					},
					_ => T_Gt(span())
				);
			},

			at('`'.code) => {
				reader.next();
				readLitsym();
			},
			
			at("$".code) => {
				reader.next();
				readAnonArg();
			},

			_ => {
				throw Lex_InvalidInput(Span.at(begin, source));
			}
		);
	}

	inline function readLSep() {
		do {
			reader.next();
			trim();
		} while(reader.hasNext() && reader.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		));

		return if(reader.eat(','.code)) {
			readCSep();
		} else {
			T_LSep(span());
		}
	}

	// Don't inline
	function readCSep() {
		trim();
		
		while(reader.hasNext() && reader.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		)) {
			reader.next();
			trim();
		};

		return T_CSep(span());
	}

	// Don't inline
	function readComma() {
		trim();

		if(reader.hasNext() && reader.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		)) {
			reader.next();
			return readCSep();
		}

		return T_Comma(span());
	}

	inline function readNumberStart() return {
		if(reader.hasNextAt(2) && reader.unsafePeekAt(1) == 'x'.code && reader.unsafePeek() == '0'.code) {
			reader.next();
			reader.next();
			reader.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					readHex();
				},
				_ => {
					throw Lex_InvalidHexStart(span());
				}
			);
		} else {
			readNumber();
		}
	}

	inline function readHex() {
		final start = reader.offset;

		do {
			reader.next();
		} while(reader.hasNext() && reader.unsafePeek()._match(
			at(('a'.code ... 'f'.code)
			 | ('A'.code ... 'F'.code)
			 | ('0'.code ... '9'.code)
			) => true,
			_ => false
		));

		reader.unsafePeek()._match(
			at(('a'.code ... 'z'.code)
			 | ('A'.code ... 'Z'.code)
			 | '_'.code
			) => {
				final end = here();

				while(reader.peekAlnumQ()) reader.next();

				final endName = here();

				throw Lex_NameAfterHex(
					new Span(begin, end, source),
					new Span(end, endName, source)
				);
			},
			_ => {
				return T_Hex(span(), reader.substring(start));
			}
		);
	}

	inline function readNumber() {
		var start = reader.offset;
		
		do {
			reader.next();
		} while(reader.peekDigit());

		final int = reader.substring(start);
		final afterDigits = here();

		final dec: Null<String> = if(reader.hasNextAt(1) && reader.unsafePeek('.'.code) && reader.unsafePeekAt(1)._match(
			at(('a'.code ... 'z'.code) | '_'.code) => false,
			_ => true
		)) {
			reader.next();
			reader.unsafePeek()._match(
				at('0'.code ... '9'.code) => {
					start = reader.offset;

					do {
						reader.next();
					} while(reader.peekDigit());

					reader.substring(start);
				},
				_ => {
					final end = here();
					
					throw Lex_IncompleteDecimalPoint(
						new Span(begin, end.advance(-1), source),
						Span.at(afterDigits, source)
					);
				}
			);
		} else {
			null;
		};

		final exp = if(reader.eat('e'.code)) {
			readExponent();
		} else {
			null;
		};

		reader.unsafePeek()._match(
			at(('a'.code ... 'z'.code)
			 | ('A'.code ... 'Z'.code)
			 | '_'.code
			) => {
				final end = here();

				while(reader.peekAlnumQ()) reader.next();

				final endName = here();

				throw Lex_NameAfterNumber(
					new Span(begin, end, source),
					new Span(end, endName, source)
				);
			},
			_ => {
				return dec._match(
					at(null) => T_Int(span(), int, exp),
					at(d!!) => T_Dec(span(), int, d, exp)
				);
			}
		);
	}

	inline function readExponent() {
		final start = reader.offset;
		final cur = reader.unsafePeek();
		final ruleBegin = here();

		if(cur == '+'.code || cur == '-'.code) {
			reader.next();
		}

		reader.unsafePeek()._match(
			at('0'.code ... '9'.code) => {
				do {
					reader.next();
				} while(reader.peekDigit());

				return reader.substring(start);
			},
			_ => {
				final end = here();

				throw Lex_MissingExponent(
					new Span(ruleBegin.advance(-1), end, source),
					new Span(end, end.advance(1), source)
				);
			}
		);
	}

	inline function readName() {
		final start = reader.offset;

		do {
			reader.next();
		} while(reader.peekAlnumQ());

		final n = reader.substring(start);

		return if(reader.eat(':'.code)) {
			T_Label(span(), n);
		} else {
			T_Name(span(), n);
		}
	}

	inline function readPunned() {
		final start = reader.offset;

		reader.unsafePeek()._match(
			at(('a'.code ... 'z'.code) | '_'.code) => {
				reader.next();
			},
			_ => {
				final end = here();

				reader.unsafePeek()._match(
					at('A'.code ... 'Z'.code) => {
						while(reader.peekAlnumQ()) reader.next();

						final endName = here();

						throw Lex_NoUppercasePunnedLabel(
							Span.at(begin, source),
							Span.at(end, source),
							new Span(end, endName, source)
						);
					},
					_ => {
						throw Lex_IncompletePunnedLabel(
							Span.at(begin, source),
							Span.at(begin.advance(1), source)
						);
					}
				);
			}
		);

		while(reader.peekAlnumQ()) {
			reader.next();
		}

		return T_Punned(span(), reader.substring(start));
	}

	inline function readTypeName() {
		final start = reader.offset;

		do {
			reader.next();
		} while(reader.peekAlnumQ());
		
		final n = reader.substring(start);

		return if(reader.eat(':'.code)) {
			final end = here();

			throw Lex_NoUppercaseLabel(
				Span.at(begin, source),
				new Span(begin.advance(1), end, source)
			);
		} else {
			T_TypeName(span(), n);
		}
	}

	inline function readLitsym() {
		final start = reader.offset;

		while(reader.peekNot('`'.code)) {
			reader.next();
		}
		
		final sym = reader.substring(start);

		reader.next();

		return T_Litsym(span(), sym);
	}

	inline function readTag() {
		final start = reader.offset;

		while(reader.peekAlnum()) {
			reader.next();
		}

		return T_Tag(span(), reader.substring(start));
	}

	inline function readChar(): Token {
		final char = switch reader.unsafePeek() {
			case '"'.code:
				final end = here();
				reader.next();
				if(reader.unsafePeek() == '"'.code) {
					reader.next();
					throw Lex_EscapeCharQuote(
						new Span(begin, end, source),
						Span.at(end, source),
						Span.at(end.advance(1), source)
					);
				} else {
					throw Lex_NoEmptyChar(new Span(begin, end, source));
				}

			case '\\'.code:
				reader.next();
				switch reader.eat() {
					case c = '\\'.code | '"'.code: c;
					case 't'.code: '\t'.code;
					case 'n'.code: '\n'.code;
					case 'r'.code: '\r'.code;
					case 'v'.code: 0x0b;
					case 'f'.code: 0x0c;
					case '0'.code: 0x00;
					case 'e'.code: 0x1b;
					case 'a'.code: 0x07;
					case 'b'.code: 0x08;
					case 'x'.code: readHexEsc();
					case 'u'.code: readUniEsc();
					case 'o'.code: readOctEsc();
					case c:
						final end = here();
						final preEnd = end.advance(-2);
						reader.next();
						throw Lex_InvalidCharEscape(
							new Span(begin, preEnd, source),
							c,
							new Span(preEnd, end, source),
							Span.at(end, source)
						);
				}
			
			default:
				reader.eat();
		};
		
		if(reader.eat('"'.code)) {
			return T_Char(span(), char);
		} else {
			final end = here();
			throw Lex_UnterminatedChar(
				new Span(begin, end, source),
				Span.at(end, source)
			);
		}
	}
	
	function readHexEsc(): Char {
		final start = reader.offset;
		
		for(_ in 0...2) {
			reader.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					reader.next();
				},
				_ => {
					final end = here();
					throw Lex_InvalidHexEscape(
						new Span(end.advance(reader.offset - start - 2), end, source),
						Span.at(end, source)
					);
				}
			);
		}
		
		return reader.substring(start).parseHex();
	}
	
	function readUniEsc(): Char {
		final start = reader.offset;

		for(_ in 0...4) {
			reader.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					reader.next();
				},
				_ => {
					final end = here();
					throw Lex_InvalidUniEscape(
						new Span(end.advance(reader.offset - start - 2), end, source),
						Span.at(end, source)
					);
				}
			);
		}

		return reader.substring(start).parseHex();
	}
	
	function readOctEsc(): Char {
		final start = reader.offset;

		for(_ in 0...3) {
			reader.unsafePeek()._match(
				at('0'.code ... '7'.code) => {
					reader.next();
				},
				_ => {
					final end = here();
					throw Lex_InvalidOctEscape(
						new Span(end.advance(reader.offset - start - 2), end, source),
						Span.at(end, source)
					);
				}
			);
		}

		return reader.substring(start).parseOctal();
	}

	inline function readStr() {
		var start = reader.offset;
		var segments = Nil;

		while(reader.hasNext()) switch reader.eat() {
			case '"'.code:
				if(start != reader.offset - 1) {
					segments = Cons(SStr(reader.substring(start, reader.offset - 1)), segments);
				}
				break;
			
			case '\\'.code:
				final end = reader.offset;
				final esc = reader.eat();
				final seg = if(esc == '('.code) {
					trim();

					var level = 1;
					var tokens = Nil;

					while(level > 0) {
						final made = readToken();

						switch made {
							case T_LParen(_) | T_HashLParen(_): level++;
							case T_RParen(_): if(--level == 0) break;
							default:
						}

						tokens = Cons(made, tokens);

						trim();
					}

					SCode(tokens);
				} else {
					final char = switch esc {
						case c = '\\'.code | '"'.code: c;
						case 't'.code: '\t'.code;
						case 'n'.code: '\n'.code;
						case 'r'.code: '\r'.code;
						case 'v'.code: 0x0b;
						case 'f'.code: 0x0c;
						case '0'.code: 0x00;
						case 'e'.code: 0x1b;
						case 'a'.code: 0x07;
						case 'b'.code: 0x08;
						case 'x'.code: readHexEsc();
						case 'u'.code: readUniEsc();
						case 'o'.code: readOctEsc();
						case c:
							final end = here();
							throw Lex_InvalidStrEscape(
								c,
								new Span(end.advance(-2), end, source) // off by 1 error?
							);
					};
					
					SChar(char);
				};
				
				if(start != end - 1) {
					segments = Cons(SStr(reader.substring(start, end - 1)), segments);
				}
				start = reader.offset;
				
				segments = Cons(seg, segments);

			case _:
		}
		
		if(!reader.hasNext()) {
			throw Lex_UnterminatedStr(Span.at(begin, source));
		}
		
		return T_Str(span(), segments);
	}

	inline function readAnonArg() {
		var depth = 0;

		while(reader.eat('.'.code)) depth++;

		reader.unsafePeek()._match(
			at('0'.code ... '9'.code) => {
				final start = reader.offset;

				do {
					reader.next();
				} while(reader.peekDigit());

				reader.unsafePeek()._match(
					at(('a'.code ... 'z'.code)
					 | ('A'.code ... 'Z'.code)
					 | '_'.code
					) => {
						final end = here();
			
						while(reader.peekAlnumQ()) reader.next();
			
						final endName = here();
			
						throw Lex_NameAfterAnonArg(
							new Span(begin, end, source),
							new Span(end, endName, source)
						);
					},
					_ => {
						return T_AnonArg(span(), depth, reader.substring(start).parseInt());
					}
				);
			},
			_ => {
				final end = here();
				throw Lex_UnterminatedAnonArg(
					new Span(begin, end, source),
					Span.at(end, source)
				);
			}
		);
	}
}