package lexing;

import text.SourceFile;
import text.Pos;
import hx.strings.Char;
import text.Span;
import lexing.Token;
import reporting.*;

using util.Strings;

class Lexer {
	static inline function range(from: Char, to: Char) {
		return [for(char in from.toInt()...(to.toInt() + 1)) char];
	}

	static final HSPACE = Charset.from(" \t");
	static final VSPACE = Charset.of(range(10, 13));
	static final DIGIT = Charset.of(range('0'.code, '9'.code));
	static final LOWER = Charset.of(range('a'.code, 'z'.code));
	static final UPPER = Charset.of(range('A'.code, 'Z'.code));
	static final ALPHA = UPPER | LOWER;
	static final ALNUM = DIGIT | LOWER | UPPER | '_'.code;
	static final XDIGIT = DIGIT | Charset.of(range('a'.code, 'f'.code).concat(range('A'.code, 'F'.code)));
	static final ODIGIT = Charset.of(range('0'.code, '7'.code));
	
	static final HSPACE_SEMI = Charset.from(" \t;");
	static final LOWER_U = LOWER | '_'.code;
	static final ALPHA_U = ALPHA | '_'.code;
	static final ALNUM_Q = ALNUM | "'".code;
	static final SINGLE_CHAR = Charset.from("()[]{}~");

	final rdr: Reader;
	final source: SourceFile;
	var begin: Pos;
	public var tokens: List<Token> = Nil;

	public function new(source: SourceFile) {
		this.source = source;
		begin = new Pos(0, 0);
		rdr = new Reader(source.text);
	}
	
	public function tokenize() {
		tokens = Cons(readToken(), null);
		
		var end = tokens;
		
		try {
			while(rdr.hasNext()) {
				final newEnd = Cons(readToken(), null);
				
				end.setTail(newEnd);
				end = newEnd;
			}
		} catch(e: Eof) {}
		
		end.setTail(Nil);
		
		tokens = retoken(tokens);
		
		return tokens;
	}


	// TODO: replace these all with indexes since those are probably faster than closures
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
		"orif" => T_Orif,
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

	static function retoken(tokens: List<Token>) {
		while(true) tokens = tokens._match(
			at([d = T_Dot(_), n = T_Name(_, _), ...rest]) => rest,
			//at([b = T_LBracket(_), ...rest]) => Cons(b, retokenGroup(rest)),
			
			at([T_Name(span, "this"), ...rest]) => {tokens.setHead(T_This(span)); rest;},
			at([T_Name(span, "true"), ...rest]) => {tokens.setHead(T_Bool(span, true)); rest;},
			at([T_Name(span, "false"), ...rest]) => {tokens.setHead(T_Bool(span, false)); rest;},
			
			at([T_Name(span, "my"), n = T_Name(_, _), ...rest]) => {
				tokens.setHead(T_My(span));
				rest;
			},
			at([T_Name(span, "has"), n = T_Name(_, _), ...rest]) => {
				tokens.setHead(T_Has(span));
				rest;
			},
			
			at([T_Name(span1, "is"), ...rest0 = [T_Name(span2, ATTRS[_] => attr!), ...rest]]) => {
				tokens.setHead(T_Is(span1));
				rest0.setHead(attr(span2));
				rest;
			},
			at([T_Name(span, KEYWORDS[_] => kw!), ...rest]) => {
				tokens.setHead(kw(span));
				tokens.setTail(retoken(rest));
				tokens;
			},
			
			at([s = T_Str(_, segs), ...rest]) => {
				retokenStr(segs);
				rest;
			},
			
			at([token, ...rest]) => rest,
			at([]) => return tokens
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
		while(rdr.peek(HSPACE_SEMI)) {
			if(rdr.eat() == ';'.code && rdr.peekNot(VSPACE)) readComment();
		}
	}

	inline function readComment() {
		if(rdr.eat('['.code)) {
			readNestedComment();
			if(rdr.peek(VSPACE)) rdr.next();
		} else {
			while(rdr.peekNot(VSPACE)) rdr.next();
		}
	}

	function readNestedComment() {
		while(true) {
			if(!rdr.hasNext()) throw "unterminated comment!";
			if(rdr.eat('['.code)) readNestedComment();
			else if(rdr.eat(']'.code)) break;
			else rdr.next();
		}
	}

	/*inline*/ function readToken() {
		begin = here();

		trim();

		final cur = (rdr.peek() : Char);

		if(VSPACE[cur]) {
			return readLSep();
		}

		begin = here();
		
		if(rdr.eat(','.code)) {
			return readComma();
		}

		return (
			if(DIGIT[cur]) {
				readNumberStart();
			}

			else if(LOWER[cur]) {
				readName();
			}

			else if(cur == '_'.code) {
				if(rdr.peekAt(1, ALNUM_Q)) {
					readName();
				} else {
					rdr.next();
					T_Wildcard(span());
				}
			}

			else if(rdr.eat(':'.code)) {
				readPunned();
			}

			else if(UPPER[cur]) {
				readTypeName();
			}

			else if(rdr.eat('.'.code)) {
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
			}

			else if(SINGLE_CHAR[cur]) {
				switch rdr.eat() {
					case '('.code: T_LParen(span());
					case ')'.code: T_RParen(span());
					case '['.code: T_LBracket(span());
					case ']'.code: T_RBracket(span());
					case '{'.code: T_LBrace(span());
					case '}'.code: T_RBrace(span());
					case '~'.code: T_Tilde(span());
					default: throw "error!";
				};
			}

			else if(rdr.eat('"'.code)) (
				if(rdr.eat('"'.code))
					T_Str(span(), [])
				else
					readStr()
			)

			else if(rdr.eat('#'.code)) (
				if(LOWER[rdr.peek()]) readTag()
				else if(rdr.eat('('.code)) T_HashLParen(span())
				else if(rdr.eat('['.code)) T_HashLBracket(span())
				else if(rdr.eat('{'.code)) T_HashLBrace(span())
				else if(rdr.eat('"'.code)) readChar()
				else throw new Diagnostic({
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
			)

			// =, =>
			else if(rdr.eat('='.code)) (
				if(rdr.eat('>'.code))
					T_EqGt(span())
				else if(rdr.eat('='.code))
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
					})
				else
					T_Eq(span())
			)

			// ?, ?=
			else if(rdr.eat('?'.code)) (
				if(rdr.eat('='.code))
					T_QuestionEq(span())
				else
					T_Question(span())
			)

			// !, !=, !!, !!=
			else if(rdr.eat('!'.code)) (
				if(rdr.eat('='.code))
					T_BangEq(span())
				else if(rdr.eat('!'.code)) (
					if(rdr.eat('='.code))
						T_BangBangEq(span())
					else
						T_BangBang(span())
				) else
					T_Bang(span())
			)

			// +, +=, ++
			else if(rdr.eat('+'.code)) (
				if(rdr.eat('='.code))
					T_PlusEq(span())
				else if(rdr.eat('+'.code))
					T_PlusPlus(span())
				else
					T_Plus(span())
			)

			// -, -=, --, ->
			else if(rdr.eat('-'.code)) (
				if(rdr.eat('='.code))
					T_MinusEq(span())
				else if(rdr.eat('-'.code)) {
					switch rdr.peek() {
						case '-'.code: {
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
						}
						case '>'.code:
							rdr.next();
							T_Cascade(span(), 2);
						default: T_MinusMinus(span());
					}
				} else if(rdr.eat('>'.code))
					T_Cascade(span(), 1)
				else
					T_Minus(span())
			)

			// *, *=, **, **=
			else if(rdr.eat('*'.code)) (
				if(rdr.eat('='.code))
					T_StarEq(span())
				else if(rdr.eat('*'.code)) (
					if(rdr.eat('='.code))
						T_StarStarEq(span())
					else
						T_StarStar(span())
				) else
					T_Star(span())
			)

			// /, /=, //, //=
			else if(rdr.eat('/'.code)) (
				if(rdr.eat('='.code))
					T_DivEq(span())
				else if(rdr.eat('/'.code)) (
					if(rdr.eat('='.code))
						T_DivDivEq(span())
					else
						T_DivDiv(span())
				) else
					T_Div(span())
			)

			// %, %=, %%, %%=
			else if(rdr.eat('%'.code)) (
				if(rdr.eat('='.code))
					T_ModEq(span())
				else if(rdr.eat('%'.code)) (
					if(rdr.eat('='.code))
						T_ModModEq(span())
					else
						T_ModMod(span())
				) else
					T_Mod(span())
			)

			// &, &=, &&, &&=
			else if(rdr.eat('&'.code)) (
				if(rdr.eat('='.code))
					T_AndEq(span())
				else if(rdr.eat('&'.code)) (
					if(rdr.eat('='.code))
						T_AndAndEq(span())
					else
						T_AndAnd(span())
				) else
					T_And(span())
			)

			// |, |=, ||, ||=
			else if(rdr.eat('|'.code)) (
				if(rdr.eat('='.code))
					T_BarEq(span())
				else if(rdr.eat('|'.code)) (
					if(rdr.eat('='.code))
						T_BarBarEq(span())
					else
						T_BarBar(span())
				) else
					T_Bar(span())
			)

			// ^, ^=, ^^, ^^=
			else if(rdr.eat('^'.code)) (
				if(rdr.eat('='.code))
					T_CaretEq(span())
				else if(rdr.eat('^'.code)) (
					if(rdr.eat('='.code))
						T_CaretCaretEq(span())
					else
						T_CaretCaret(span())
				) else
					T_Caret(span())
			)

			// <, <=, <<, <<=
			else if(rdr.eat('<'.code)) (
				if(rdr.eat('='.code))
					T_LtEq(span())
				else if(rdr.eat('<'.code)) (
					if(rdr.eat('='.code))
						T_LtLtEq(span())
					else
						T_LtLt(span())
				) else
					T_Lt(span())
			)

			// >, >=, >>, >>=
			else if(rdr.eat('>'.code)) (
				if(rdr.eat('='.code))
					T_GtEq(span())
				else if(rdr.eat('>'.code)) (
					if(rdr.eat('='.code))
						T_GtGtEq(span())
					else
						T_GtGt(span())
				) else
					T_Gt(span())
			)

			else if(rdr.eat('`'.code)) {
				readLitsym();
			}
			
			else if(rdr.eat("$".code)) {
				readAnonArg();
			}

			else if(!rdr.hasNext()) {
				throw new Eof();
			}

			else {
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
		} while(rdr.peek(VSPACE));

		return if(rdr.eat(','.code)) {
			readCSep();
		} else {
			T_LSep(span());
		}
	}

	// Don't inline
	function readCSep() {
		trim();
		
		while(rdr.peek(VSPACE)) {
			rdr.next();
			trim();
		};

		return T_CSep(span());
	}

	// Don't inline
	function readComma() {
		trim();

		if(rdr.peek(VSPACE)) {
			rdr.next();
			return readCSep();
		}

		return T_Comma(span());
	}

	inline function readNumberStart() {
		if(rdr.eat("0x")) {
			if(rdr.peek(XDIGIT)) {
				return readHex();
			} else {
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
		} else {
			return readNumber();
		}
	}

	inline function readHex() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.peek(XDIGIT));

		if(rdr.peek(ALPHA_U)) {
			final end = here();

			while(rdr.peek(ALNUM_Q)) rdr.next();

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
		} else {
			return T_Hex(span(), rdr.substring(start));
		}
	}

	inline function readNumber() {
		var start = rdr.offset;
		
		do {
			rdr.next();
		} while(rdr.peek(DIGIT));

		final int = rdr.substring(start);
		final afterDigits = here();

		final dec = if(rdr.peek('.'.code) && rdr.peekNotAt(1, LOWER_U)) {
			rdr.next();

			if(rdr.peek(DIGIT)) {
				start = rdr.offset;

				do {
					rdr.next();
				} while(rdr.peek(DIGIT));

				Some(rdr.substring(start));
			} else {
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
		} else {
			None;
		};

		final exp = if(rdr.eat('e'.code)) {
			Some(readExponent());
		} else {
			None;
		};

		if(rdr.peek(ALPHA_U)) {
			final end = here();

			while(rdr.peek(ALNUM_Q)) rdr.next();

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
		} else {
			return switch dec {
				case None: T_Int(span(), int, exp);
				case Some(d): T_Dec(span(), int, d, exp);
			}
		}
	}

	inline function readExponent() {
		final start = rdr.offset;
		final cur = rdr.peek();
		final ruleBegin = here();

		if(cur == '+'.code || cur == '-'.code) {
			rdr.next();
		}

		if(rdr.peek(DIGIT)) {
			do {
				rdr.next();
			} while(rdr.peek(DIGIT));

			return rdr.substring(start);
		} else {
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
	}

	inline function readName() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.peek(ALNUM_Q));

		final n = rdr.substring(start);

		return if(rdr.eat(':'.code)) {
			T_Label(span(), n);
		} else {
			T_Name(span(), n);
		}
	}

	inline function readPunned() {
		final start = rdr.offset;

		if(rdr.peek(LOWER_U)) {
			rdr.next();
		} else {
			final end = here();

			if(rdr.peek(UPPER)) {
				while(rdr.peek(ALNUM_Q)) rdr.next();

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
			} else {
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
		}

		while(rdr.peek(ALNUM_Q)) {
			rdr.next();
		}

		return T_Punned(span(), rdr.substring(start));
	}

	inline function readTypeName() {
		final start = rdr.offset;

		do {
			rdr.next();
		} while(rdr.peek(ALNUM_Q));
		
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

		while(rdr.peek(ALNUM)) {
			rdr.next();
		}

		return T_Tag(span(), rdr.substring(start));
	}

	inline function readChar(): Token {
		final char = switch rdr.peek() {
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
					case c = "\\".code | "\"".code: c;
					case "t".code: "\t".code;
					case "n".code: "\n".code;
					case "r".code: "\r".code;
					case "v".code: 0x0b;
					case "f".code: 0x0c;
					case "0".code: 0x00;
					case "e".code: 0x1b;
					case "a".code: 0x07;
					case "b".code: 0x08;
					case "x".code: readHexEsc();
					case "u".code: readUniEsc();
					case "o".code: readOctEsc();
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
			if(rdr.peek(XDIGIT)) {
				rdr.next();
			} else {
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
		}
		
		return rdr.substring(start).parseHex();
	}
	
	function readUniEsc(): Char {
		final start = rdr.offset;

		for(_ in 0...4) {
			if(rdr.peek(XDIGIT)) {
				rdr.next();
			} else {
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
		}

		return rdr.substring(start).parseHex();
	}
	
	function readOctEsc(): Char {
		final start = rdr.offset;

		for(_ in 0...3) {
			if(rdr.peek(ODIGIT)) {
				rdr.next();
			} else {
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
						case c = "\\".code | "\"".code: c;
						case "t".code: "\t".code;
						case "n".code: "\n".code;
						case "r".code: "\r".code;
						case "v".code: 0x0b;
						case "f".code: 0x0c;
						case "0".code: 0x00;
						case "e".code: 0x1b;
						case "a".code: 0x07;
						case "b".code: 0x08;
						case "x".code: readHexEsc();
						case "u".code: readUniEsc();
						case "o".code: readOctEsc();
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

		if(rdr.peek(DIGIT)) {
			final start = rdr.offset;

			do {
				rdr.next();
			} while(rdr.peek(DIGIT));

			if(rdr.peek(ALPHA_U)) {
				final end = here();
	
				while(rdr.peek(ALNUM_Q)) rdr.next();
	
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
			} else {
				return T_AnonArg(span(), depth, rdr.substring(start).parseInt());
			}
		} else {
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
	}
}