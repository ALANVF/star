package lexing;

import hl.NativeArray;
import text.SourceFile;
import text.Pos;
import hx.strings.Char;
import text.Span;
import lexing.Token;
import reporting.*;
import util.Enums.getIndex as getEnumIndex;

using util.Strings;

class Lexer {
	final rdr: Reader;
	final source: SourceFile;
	var begin: Pos;

	public inline function new(source: SourceFile) {
		this.source = source;
		begin = new Pos(0, 0);
		rdr = new Reader(source.text);
	}
	
	public function tokenize() {
		var tokens = Cons(readToken(), null);
		
		var end = tokens;
		
		try {
			while(rdr.hasNext()) {
				final newEnd = Cons(readToken(), null);
				
				end.unsafeSetTail(newEnd);
				end = newEnd;
			}
		} catch(_: Eof) {}
		
		end.unsafeSetTail(Nil);
		
		return retoken(tokens);
	}


	// TODO: replace these all with indexes since those are probably faster than closures
	static final KEYWORDS = [
		"module" => getEnumIndex(Token, T_Module),
		"my" => getEnumIndex(Token, T_My),
		"on" => getEnumIndex(Token, T_On),
		"return" => getEnumIndex(Token, T_Return),
		"init" => getEnumIndex(Token, T_Init),
		"deinit" => getEnumIndex(Token, T_Deinit),
		"operator" => getEnumIndex(Token, T_Operator),
		"class" => getEnumIndex(Token, T_Class),
		"alias" => getEnumIndex(Token, T_Alias),
		"type" => getEnumIndex(Token, T_Type),
		"kind" => getEnumIndex(Token, T_Kind),
		"category" => getEnumIndex(Token, T_Category),
		"protocol" => getEnumIndex(Token, T_Protocol),
		"is" => getEnumIndex(Token, T_Is),
		"of" => getEnumIndex(Token, T_Of),
		"use" => getEnumIndex(Token, T_Use),
		"has" => getEnumIndex(Token, T_Has),
		"if" => getEnumIndex(Token, T_If),
		"else" => getEnumIndex(Token, T_Else),
		"while" => getEnumIndex(Token, T_While),
		"for" => getEnumIndex(Token, T_For),
		"do" => getEnumIndex(Token, T_Do),
		"case" => getEnumIndex(Token, T_Case),
		"match" => getEnumIndex(Token, T_Match),
		"at" => getEnumIndex(Token, T_At),
		"break" => getEnumIndex(Token, T_Break),
		"next" => getEnumIndex(Token, T_Next),
		"throw" => getEnumIndex(Token, T_Throw),
		"try" => getEnumIndex(Token, T_Try),
		"catch" => getEnumIndex(Token, T_Catch)
	];

	static final ATTRS = [
		"static" => getEnumIndex(Token, T_Static),
		"hidden" => getEnumIndex(Token, T_Hidden),
		"readonly" => getEnumIndex(Token, T_Readonly),
		"friend" => getEnumIndex(Token, T_Friend),
		"unordered" => getEnumIndex(Token, T_Unordered),
		"getter" => getEnumIndex(Token, T_Getter),
		"setter" => getEnumIndex(Token, T_Setter),
		"main" => getEnumIndex(Token, T_Main),
		"inline" => getEnumIndex(Token, T_Inline),
		"noinherit" => getEnumIndex(Token, T_Noinherit),
		"pattern" => getEnumIndex(Token, T_Pattern),
		"asm" => getEnumIndex(Token, T_Asm),
		"native" => getEnumIndex(Token, T_Native),
		"flags" => getEnumIndex(Token, T_Flags),
		"uncounted" => getEnumIndex(Token, T_Uncounted),
		"strong" => getEnumIndex(Token, T_Strong),
		"sealed" => getEnumIndex(Token, T_Sealed),
		"macro" => getEnumIndex(Token, T_Macro)
	];

	static function retoken(tokens: List<Token>) {
		final ogTokens = tokens;
		final tokenType = hl.Type.get((null : Token));
		final tokenArgs = new hl.NativeArray<Span>(1);

		while(true) tokens = tokens._match(
			at([T_Dot(_), T_Name(_, _), ...rest]) => rest,
			//at([b = T_LBracket(_), ...rest]) => Cons(b, retokenGroup(rest)),
			
			at([T_Name(span, "this"), ...rest]) => { tokens.unsafeSetHead(T_This(span)); rest;},
			at([T_Name(span, "true"), ...rest]) => { tokens.unsafeSetHead(T_Bool(span, true)); rest; },
			at([T_Name(span, "false"), ...rest]) => { tokens.unsafeSetHead(T_Bool(span, false)); rest; },
			
			at([T_Name(span, "my"), T_Name(_, _), ...rest]) => {
				tokens.unsafeSetHead(T_My(span));
				rest;
			},
			at([T_Name(span, "has"), T_Name(_, _), ...rest]) => {
				tokens.unsafeSetHead(T_Has(span));
				rest;
			},
			
			at([T_Name(span1, "is"), ...rest0 = [T_Name(span2, ATTRS[_] => attrIndex!), ...rest]]) => {
				tokens.unsafeSetHead(T_Is(span1));
				tokenArgs[0] = span2;
				rest0.unsafeSetHead((tokenType.allocEnum(attrIndex, tokenArgs, 1) : Token));
				rest;
			},
			at([T_Name(span, KEYWORDS[_] => kwIndex!), ...rest]) => {
				tokenArgs[0] = span;
				tokens.unsafeSetHead((tokenType.allocEnum(kwIndex, tokenArgs, 1) : Token));
				rest;
			},
			
			at([T_Str(_, segs), ...rest]) => {
				retokenStr(segs);
				rest;
			},
			
			at([_, ...rest]) => rest,
			at([]) => return ogTokens
		);
	}

	/*function retokenGroup(tokens: List<Token>) return tokens._match(
		at([t = T_Dot(_) | T_TypeName(_, _) | T_LSep(_), ...rest]) => Cons(t, retokenGroup(rest)),
		at([n = T_Name(_, _), ...(rest = (Cons(T_LSep(_), Cons(T_RBracket(_), _)) | Cons(T_RBracket(_), _)))]) => Cons(n, retoken(rest)),
		at(rest) => retoken(rest)
	);*/

	static function retokenStr(segs: Array<StrSegment>) {
		for(i in 0...segs.length) switch segs[i] {
			case SCode(strTokens): segs[i] = SCode(retoken(strTokens));
			default:
		};
	}


	inline function here() {
		return rdr.cursor.pos();
	}

	inline function span() {
		return new Span(begin, here(), source);
	}

	inline function trim() {
		while(rdr.hasNext()) rdr.unsafePeek()._match(
			at(' '.code | '\t'.code) => rdr.next(),
			at(';'.code) => {
				rdr.next();
				if(rdr.hasNext()) rdr.unsafePeek()._match(
					at('\n'.code ... '\r'.code) => break,
					_ => readComment()
				);
			},
			_ => break
		);
	}

	inline function readComment() {
		if(rdr.eat('['.code)) {
			readNestedComment();
			if(rdr.hasNext()) rdr.unsafePeek()._match(
				at('\n'.code ... '\r'.code) => rdr.next(),
				_ => {}
			);
		} else {
			while(rdr.hasNext()) rdr.unsafePeek()._match(
				at('\n'.code ... '\r'.code) => break,
				_ => rdr.next()
			);
		}
	}

	function readNestedComment(): Void {
		while(rdr.hasNext()) rdr.eat()._match(
			at('['.code) => readNestedComment(),
			at(']'.code) => return,
			_ => {}
		);
		
		throw "unterminated comment!";
	}

	/*inline*/ function readToken(): Token {
		final oldBegin = begin = here();

		trim();

		if(!rdr.hasNext()) {
			throw new Eof();
		}

		final cur = rdr.unsafePeek();
		
		begin = here();

		return cur._match(
			at('\n'.code ... '\r'.code) => {
				begin = oldBegin;
				readLSep();
			},

			at(','.code) => {
				rdr.next();
				readComma();
			},

			at('0'.code ... '9'.code) => {
				readNumberStart();
			},

			at('a'.code ... 'z'.code) => {
				readName();
			},

			at('_'.code) => {
				rdr.unsafePeekAt(1)._match(
					at(('a'.code ... 'z'.code)
					 | ('A'.code ... 'Z'.code)
					 | ('0'.code ... '9'.code)
					 | '_'.code
					 | "'".code
					) => {
						readName();
					},
					_ => {
						rdr.next();
						T_Wildcard(span());
					}
				);
			},

			at(':'.code) => {
				rdr.next();
				readPunned();
			},

			at('A'.code ... 'Z'.code) => {
				readTypeName();
			},

			at('.'.code) => {
				rdr.next();
				if(rdr.eat('.'.code)) {
					if(rdr.eat('.'.code)) {
						T_DotDotDot(span());
					} else {
						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Syntax error",
							info: [
								Spanned({
									span: span(),
									message: 'Invalid operator `..`',
									isPrimary: true
								}),
							]
						});
					}
				} else {
					T_Dot(span());
				}
			},

			at('('.code) => { rdr.next(); T_LParen(span()); },
			at(')'.code) => { rdr.next(); T_RParen(span()); },
			at('['.code) => { rdr.next(); T_LBracket(span()); },
			at(']'.code) => { rdr.next(); T_RBracket(span()); },
			at('{'.code) => { rdr.next(); T_LBrace(span()); },
			at('}'.code) => { rdr.next(); T_RBrace(span()); },
			at('~'.code) => { rdr.next(); T_Tilde(span()); },
			
			at('"'.code) => {
				rdr.next();
				if(rdr.eat('"'.code))
					T_Str(span(), []);
				else
					readStr();
			},

			at('#'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('a'.code ... 'z'.code) => readTag(),
					at('('.code) => { rdr.next(); T_HashLParen(span()); },
					at('['.code) => { rdr.next(); T_HashLBracket(span()); },
					at('{'.code) => { rdr.next(); T_HashLBrace(span()); },
					at('"'.code) => { rdr.next(); readChar(); },
					_ => throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Syntax error",
						info: [
							Spanned({
								span: Span.at(here(), source),
								message: 'Unexpected `${rdr.peek()}` after `#`',
								isPrimary: true
							}),
							Spanned({
								span: Span.at(begin, source),
								isSecondary: true
							})
						]
					})
				);
			},

			// =, =>
			at('='.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('>'.code) => { rdr.next(); T_EqGt(span()); },
					at('='.code) => {
						rdr.next();
						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Syntax error",
							info: [
								Spanned({
									span: span(),
									message: "Please use `?=` instead of `==` in Star",
									isPrimary: true
								})
							]
						});
					},
					_ => T_Eq(span())
				);
			},

			// ?, ?=
			at('?'.code) => {
				rdr.next();
				if(rdr.eat('='.code))
					T_QuestionEq(span())
				else
					T_Question(span());
			},

			// !, !=, !!, !!=
			at('!'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_BangEq(span()); },
					at('!'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_BangBangEq(span())
						else
							T_BangBang(span());
					},
					_ => T_Bang(span())
				);
			},

			// +, +=, ++
			at('+'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_PlusEq(span()); },
					at('+'.code) => { rdr.next(); T_PlusPlus(span()); },
					_ => T_Plus(span())
				);
			},

			// -, -=, --, ->
			at('-'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_MinusEq(span()); },
					at('-'.code) => {
						rdr.next();
						rdr.unsafePeek()._match(
							at('-'.code) => {
								rdr.next();
								
								var depth = 2;

								while(rdr.eat('-'.code)) depth++;

								if(rdr.eat('>'.code)) {
									T_Cascade(span(), depth);
								} else {
									final end = here();
									throw new Diagnostic({
										severity: Severity.ERROR,
										message: "Unterminated cascade",
										info: [
											Spanned({
												span: Span.at(end, source),
												message: "Expected a `>` to finish the cascade operator",
												isPrimary: true
											}),
											Spanned({
												span: new Span(begin, end, source),
												isSecondary: true
											})
										]
									});
								}
							},
							at('>'.code) => { rdr.next(); T_Cascade(span(), 2); },
							_ => T_MinusMinus(span())
						);
					},
					at('>'.code) => { rdr.next(); T_Cascade(span(), 1); },
					_ => T_Minus(span())
				);
			},

			// *, *=, **, **=
			at('*'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_StarEq(span()); },
					at('*'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_StarStarEq(span())
						else
							T_StarStar(span());
					},
					_ => T_Star(span())
				);
			},

			// /, /=, //, //=
			at('/'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_DivEq(span()); },
					at('/'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_DivDivEq(span())
						else
							T_DivDiv(span());
					},
					_ => T_Div(span())
				);
			},

			// %, %=, %%, %%=
			at('%'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_ModEq(span()); },
					at('%'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_ModModEq(span())
						else
							T_ModMod(span());
					},
					_ => T_Mod(span())
				);
			},

			// &, &=, &&, &&=
			at('&'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_AndEq(span()); },
					at('&'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_AndAndEq(span())
						else
							T_AndAnd(span());
					},
					_ => T_And(span())
				);
			},

			// |, |=, ||, ||=
			at('|'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_BarEq(span()); },
					at('|'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_BarBarEq(span())
						else
							T_BarBar(span());
					},
					_ => T_Bar(span())
				);
			},

			// ^, ^=, ^^, ^^=
			at('^'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_CaretEq(span()); },
					at('^'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_CaretCaretEq(span())
						else
							T_CaretCaret(span());
					},
					_ => T_Caret(span())
				);
			},

			// <, <=, <<, <<=
			at('<'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_LtEq(span()); },
					at('<'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_LtLtEq(span())
						else
							T_LtLt(span());
					},
					_ => T_Lt(span())
				);
			},

			// >, >=, >>, >>=
			at('>'.code) => {
				rdr.next();
				rdr.unsafePeek()._match(
					at('='.code) => { rdr.next(); T_GtEq(span()); },
					at('>'.code) => {
						rdr.next();
						if(rdr.eat('='.code))
							T_GtGtEq(span())
						else
							T_GtGt(span());
					},
					_ => T_Gt(span())
				);
			},

			at('`'.code) => {
				rdr.next();
				readLitsym();
			},
			
			at("$".code) => {
				rdr.next();
				readAnonArg();
			},

			_ => {
				throw new Diagnostic({
					severity: Severity.ERROR,
					message: "Syntax error",
					info: [
						Spanned({
							span: Span.at(begin, source),
							message: "This is not the syntax that you are looking for",
							isPrimary: true
						})
					]
				});
			}
		);
	}

	inline function readLSep() {
		do {
			rdr.next();
			trim();
		} while(rdr.hasNext() && rdr.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		));

		return if(rdr.eat(','.code)) {
			readCSep();
		} else {
			T_LSep(span());
		}
	}

	// Don't inline
	function readCSep() {
		trim();
		
		while(rdr.hasNext() && rdr.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		)) {
			rdr.next();
			trim();
		};

		return T_CSep(span());
	}

	// Don't inline
	function readComma() {
		trim();

		if(rdr.hasNext() && rdr.unsafePeek()._match(
			at('\n'.code ... '\r'.code) => true,
			_ => false
		)) {
			rdr.next();
			return readCSep();
		}

		return T_Comma(span());
	}

	inline function readNumberStart() return {
		if(rdr.hasNextAt(2) && rdr.unsafePeekAt(1) == 'x'.code && rdr.unsafePeek() == '0'.code) {
			rdr.next();
			rdr.next();
			rdr.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					readHex();
				},
				_ => {
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Unexpected start of hexdecimal literal",
						info: [
							Spanned({
								span: span(),
								message: "Were you wanting a hexdecimal literal here or what?",
								isPrimary: true
							})
						]
					});
				}
			);
		} else {
			readNumber();
		}
	}

	inline function readHex() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.hasNext() && rdr.unsafePeek()._match(
			at(('a'.code ... 'f'.code)
			 | ('A'.code ... 'F'.code)
			 | ('0'.code ... '9'.code)
			) => true,
			_ => false
		));

		rdr.unsafePeek()._match(
			at(('a'.code ... 'z'.code)
			 | ('A'.code ... 'Z'.code)
			 | '_'.code
			) => {
				final end = here();

				while(rdr.peekAlnumQ()) rdr.next();

				final endName = here();

				throw new Diagnostic({
					severity: Severity.ERROR,
					message: "Invalid hexdecimal literal",
					info: [
						Spanned({
							span: new Span(end, endName, source),
							message: "Make sure to separate names from numbers",
							isPrimary: true
						}),
						Spanned({
							span: new Span(begin, end, source),
							isSecondary: true
						})
					]
				});
			},
			_ => {
				return T_Hex(span(), rdr.substring(start));
			}
		);
	}

	inline function readNumber() {
		var start = rdr.offset;
		
		do {
			rdr.next();
		} while(rdr.peekDigit());

		final int = rdr.substring(start);
		final afterDigits = here();

		final dec = if(rdr.hasNextAt(1) && rdr.unsafePeek('.'.code) && rdr.unsafePeekAt(1)._match(
			at(('a'.code ... 'z'.code) | '_'.code) => false,
			_ => true
		)) {
			rdr.next();
			rdr.unsafePeek()._match(
				at('0'.code ... '9'.code) => {
					start = rdr.offset;

					do {
						rdr.next();
					} while(rdr.peekDigit());

					Some(rdr.substring(start));
				},
				_ => {
					final end = here();
					
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid decimal literal",
						info: [
							Spanned({
								span: Span.at(afterDigits, source),
								message: "At least 1 digit is required on both sides of the decimal point",
								isPrimary: true
							}),
							Spanned({
								span: new Span(begin, end.advance(-1), source),
								isSecondary: true
							})
						]
					});
				}
			);
		} else {
			None;
		};

		final exp = if(rdr.eat('e'.code)) {
			Some(readExponent());
		} else {
			None;
		};

		rdr.unsafePeek()._match(
			at(('a'.code ... 'z'.code)
			 | ('A'.code ... 'Z'.code)
			 | '_'.code
			) => {
				final end = here();

				while(rdr.peekAlnumQ()) rdr.next();

				final endName = here();

				throw new Diagnostic({
					severity: Severity.ERROR,
					message: "Invalid number literal",
					info: [
						Spanned({
							span: new Span(end, endName, source),
							message: "Make sure to separate names from numbers",
							isPrimary: true
						}),
						Spanned({
							span: new Span(begin, end, source),
							isSecondary: true
						})
					]
				});
			},
			_ => {
				return switch dec {
					case None: T_Int(span(), int, exp);
					case Some(d): T_Dec(span(), int, d, exp);
				}
			}
		);
	}

	inline function readExponent() {
		final start = rdr.offset;
		final cur = rdr.unsafePeek();
		final ruleBegin = here();

		if(cur == '+'.code || cur == '-'.code) {
			rdr.next();
		}

		rdr.unsafePeek()._match(
			at('0'.code ... '9'.code) => {
				do {
					rdr.next();
				} while(rdr.peekDigit());

				return rdr.substring(start);
			},
			_ => {
				final end = here();

				throw new Diagnostic({
					severity: Severity.ERROR,
					message: "Invalid number literal",
					info: [
						Spanned({
							span: new Span(end, end.advance(1), source),
							message: "Expected a number after the exponent indicator",
							isPrimary: true
						}),
						Spanned({
							span: new Span(ruleBegin.advance(-1), end, source),
							message: "This indicates that the number has an exponent",
							isSecondary: true
						})
					]
				});
			}
		);
	}

	inline function readName() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.peekAlnumQ());

		final n = rdr.substring(start);

		return if(rdr.eat(':'.code)) {
			T_Label(span(), n);
		} else {
			T_Name(span(), n);
		}
	}

	inline function readPunned() {
		final start = rdr.offset;

		rdr.unsafePeek()._match(
			at(('a'.code ... 'z'.code) | '_'.code) => {
				rdr.next();
			},
			_ => {
				final end = here();

				rdr.unsafePeek()._match(
					at('A'.code ... 'Z'.code) => {
						while(rdr.peekAlnumQ()) rdr.next();

						final endName = here();

						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Invalid punned label",
							info: [
								Spanned({
									span: Span.at(end, source),
									message: "Punned labels may not start with an uppercase letter",
									isPrimary: true
								}),
								Spanned({
									span: Span.at(begin, source),
									isSecondary: true
								}),
								Spanned({
									span: new Span(end, endName, source),
									isSecondary: true
								})
							]
						});
					},
					_ => {
						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Invalid punned label",
							info: [
								Spanned({
									span: Span.at(begin.advance(1), source),
									message: "Was expecting a name for the punned label",
									isPrimary: true
								}),
								Spanned({
									span: Span.at(begin, source),
									isSecondary: true
								})
							]
						});
					}
				);
			}
		);

		while(rdr.peekAlnumQ()) {
			rdr.next();
		}

		return T_Punned(span(), rdr.substring(start));
	}

	inline function readTypeName() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.peekAlnumQ());
		
		final n = rdr.substring(start);

		return if(rdr.eat(':')) {
			final end = here();

			throw new Diagnostic({
				severity: Severity.ERROR,
				message: "Invalid label",
				info: [
					Spanned({
						span: Span.at(begin, source),
						message: "Labels may not start with an uppercase letter",
						isPrimary: true
					}),
					Spanned({
						span: new Span(begin.advance(1), end, source),
						isSecondary: true
					})
				]
			});
		} else {
			T_TypeName(span(), n);
		}
	}

	inline function readLitsym() {
		final start = rdr.offset;

		while(rdr.peekNot('`'.code)) {
			rdr.next();
		}
		
		final sym = rdr.substring(start);

		rdr.next();

		return T_Litsym(span(), sym);
	}

	inline function readTag() {
		final start = rdr.offset;

		while(rdr.peekAlnum()) {
			rdr.next();
		}

		return T_Tag(span(), rdr.substring(start));
	}

	inline function readChar(): Token {
		final char = switch rdr.unsafePeek() {
			case '"'.code:
				final end = here();
				if(rdr.peekAt(1, '"'.code)) {
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid char literal",
						info: [
							Spanned({
								span: Span.at(end, source),
								message: "`\"` characters need to be escaped in char literals",
								isPrimary: true
							}),
							Spanned({
								span: new Span(begin, end, source),
								isSecondary: true
							}),
							Spanned({
								span: Span.at(end.advance(1), source),
								isSecondary: true
							})
						]
					});
				} else {
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid char literal",
						info: [
							Spanned({
								span: new Span(begin, end.advance(1), source),
								message: "Char literals may not be empty",
								isPrimary: true
							})
						]
					});
				}

			case '\\'.code:
				rdr.next();
				switch rdr.eat() {
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
						final end = here().advance(-1);
						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Invalid escape character",
							info: [
								// off by 1 errors?
								Spanned({
									span: new Span(here().advance(-2), here(), source),
									message: 'Escape character `$c` ' + (
										if(c == '('.code) "is not allowed in char literals"
										else "does not exist"
									),
									isPrimary: true
								}),
								Spanned({
									span: new Span(begin, end.advance(-1), source),
									isSecondary: true
								}),
								Spanned({
									span: Span.at(end.advance(), source),
									isSecondary: true
								})
							]
						});
				}
			
			default:
				rdr.eat();
		};
		
		if(rdr.eat('"'.code)) {
			return T_Char(span(), char);
		} else {
			final end = here();
			throw new Diagnostic({
				severity: Severity.ERROR,
				message: "Unterminated char literal",
				info: [
					Spanned({
						span: Span.at(end, source),
						message: "Expected another `\"` to finish the char literal",
						isPrimary: true
					}),
					Spanned({
						span: new Span(begin, end, source),
						isSecondary: true
					})
				]
			});
		}
	}
	
	function readHexEsc(): Char {
		final start = rdr.offset;
		
		for(_ in 0...2) {
			rdr.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					rdr.next();
				},
				_ => {
					final end = here();
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid hexdecimal escape code",
						info: [
							Spanned({
								span: Span.at(end, source),
								message: "Was expecting a hexdecimal digit here",
								isPrimary: true
							}),
							Spanned({
								span: new Span(end.advance(rdr.offset - start - 2), end, source),
								isSecondary: true
							})
						]
					});
				}
			);
		}
		
		return rdr.substring(start).parseHex();
	}
	
	function readUniEsc(): Char {
		final start = rdr.offset;

		for(_ in 0...4) {
			rdr.unsafePeek()._match(
				at(('a'.code ... 'f'.code)
				 | ('A'.code ... 'F'.code)
				 | ('0'.code ... '9'.code)
				) => {
					rdr.next();
				},
				_ => {
					final end = here();
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid unicode escape code",
						info: [
							Spanned({
								span: Span.at(end, source),
								message: "Was expecting a hexdecimal digit here",
								isPrimary: true
							}),
							Spanned({
								span: new Span(end.advance(rdr.offset - start - 2), end, source),
								isSecondary: true
							})
						]
					});
				}
			);
		}

		return rdr.substring(start).parseHex();
	}
	
	function readOctEsc(): Char {
		final start = rdr.offset;

		for(_ in 0...3) {
			rdr.unsafePeek()._match(
				at('0'.code ... '7'.code) => {
					rdr.next();
				},
				_ => {
					final end = here();
					throw new Diagnostic({
						severity: Severity.ERROR,
						message: "Invalid octal escape code",
						info: [
							Spanned({
								span: Span.at(end, source),
								message: "Was expecting an octal digit here",
								isPrimary: true
							}),
							Spanned({
								span: new Span(end.advance(rdr.offset - start - 2), end, source),
								isSecondary: true
							})
						]
					});
				}
			);
		}

		return rdr.substring(start).parseOctal();
	}

	inline function readStr() {
		var start = rdr.offset;
		final segments = [];

		while(rdr.hasNext()) switch rdr.eat() {
			case '"'.code:
				if(start != rdr.offset - 1) segments.push(SStr(rdr.substring(start, rdr.offset - 1)));
				break;
			
			case '\\'.code:
				final end = rdr.offset;
				final esc = rdr.eat();
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

					SCode(tokens.rev());
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
							final end = here().advance(-1);
							throw new Diagnostic({
								severity: Severity.ERROR,
								message: "Invalid escape character",
								info: [
									Spanned({
										span: new Span(end.advance(-1), end.advance(), source), // off by 1 error?
										message: 'Escape character `\\$c` does not exist',
										isPrimary: true
									})
								]
							});
					};
					
					SChar(char);
				};
				
				if(start != end - 1) {
					segments.push(SStr(rdr.substring(start, end - 1)));
					start = rdr.offset;
				}
				
				segments.push(seg);

			case _:
		}

		if(!rdr.hasNext()) {
			throw new Diagnostic({
				severity: Severity.ERROR,
				message: "Unterminated string",
				info: [
					Spanned({
						span: Span.at(begin, source),
						message: "This string is never terminated",
						isPrimary: true
					})
				]
			});
		}
		
		return T_Str(span(), segments);
	}

	inline function readAnonArg() {
		var depth = 0;

		while(rdr.eat('.'.code)) depth++;

		rdr.unsafePeek()._match(
			at('0'.code ... '9'.code) => {
				final start = rdr.offset;

				do {
					rdr.next();
				} while(rdr.peekDigit());

				rdr.unsafePeek()._match(
					at(('a'.code ... 'z'.code)
					 | ('A'.code ... 'Z'.code)
					 | '_'.code
					) => {
						final end = here();
			
						while(rdr.peekAlnumQ()) rdr.next();
			
						final endName = here();
			
						throw new Diagnostic({
							severity: Severity.ERROR,
							message: "Invalid anonymous argument",
							info: [
								Spanned({
									span: new Span(end, endName, source),
									message: "Make sure to separate names from numbers",
									isPrimary: true
								}),
								Spanned({
									span: new Span(begin, end, source),
									isSecondary: true
								})
							]
						});
					},
					_ => {
						return T_AnonArg(span(), depth, rdr.substring(start).parseInt());
					}
				);
			},
			_ => {
				final end = here();
				throw new Diagnostic({
					severity: Severity.ERROR,
					message: "Unterminated anonymous argument",
					info: [
						Spanned({
							span: Span.at(end, source),
							message: "Was expecting a number here",
							isPrimary: true
						}),
						Spanned({
							span: new Span(begin, end, source),
							isSecondary: true
						})
					]
				});
			}
		);
	}
}