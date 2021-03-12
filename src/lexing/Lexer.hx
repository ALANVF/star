package lexing;

import text.SourceFile;
import text.Pos;
import hx.strings.Char;
import text.Span;
import lexing.Token;
import reporting.*;
import util.Buffer;

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
		try {
			while(rdr.hasNext()) {
				tokens = tokens.prepend(readToken());
			}
		} catch(e: Eof) {
			trace(e.posInfos);
			trace(e.stack);
		}

		return tokens = retoken(tokens.rev());
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

	function retoken(tokens: List<Token>) return Util.match(tokens,
		at([d = T_Dot(_), n = T_Name(_, _), ...rest]) => List.of(d, n, ...retoken(rest)),
		//at([b = T_LBracket(_), ...rest]) => Cons(b, retokenGroup(rest)),
		
		at([T_Name(span, "this"), ...rest]) => Cons(T_This(span), retoken(rest)),
		at([T_Name(span, "true"), ...rest]) => Cons(T_Bool(span, true), retoken(rest)),
		at([T_Name(span, "false"), ...rest]) => Cons(T_Bool(span, false), retoken(rest)),
		
		at([T_Name(span, "my"), n = T_Name(_, _), ...rest]) => List.of(T_My(span), n, ...retoken(rest)),
		at([T_Name(span, "has"), n = T_Name(_, _), ...rest]) => List.of(T_Has(span), n, ...retoken(rest)),
		
		at([T_Name(span1, "is"), T_Name(span2, ATTRS[_] => attr), ...rest], when(attr != null)) => List.of(T_Is(span1), attr(span2), ...retoken(rest)),
		at([T_Name(span, KEYWORDS[_] => kw), ...rest], when(kw != null)) => Cons(kw(span), retoken(rest)),
		
		at([s = T_Str(_, segs), ...rest]) => {
			retokenStr(segs);
			Cons(s, retoken(rest));
		},
		
		at([token, ...rest]) => Cons(token, retoken(rest)),
		at([]) => Nil
	);

	/*function retokenGroup(tokens: List<Token>) return Util.match(tokens,
		at([t = T_Dot(_) | T_TypeName(_, _) | T_LSep(_), ...rest]) => Cons(t, retokenGroup(rest)),
		at([n = T_Name(_, _), ...(rest = (Cons(T_LSep(_), Cons(T_RBracket(_), _)) | Cons(T_RBracket(_), _)))]) => Cons(n, retoken(rest)),
		at(rest) => retoken(rest)
	);*/

	function retokenStr(segs: Array<StrSegment>) {
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
		} else {
			while(rdr.peekNot(VSPACE)) rdr.next();
		}

		if(rdr.hasNext()) rdr.next();
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
		final hex = new Buffer();

		do {
			hex.addChar(rdr.eat());
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
			return T_Hex(span(), hex.toString());
		}
	}

	inline function readNumber() {
		final int = new Buffer();
		
		do {
			int.addChar(rdr.eat());
		} while(rdr.peek(DIGIT));

		final afterDigits = here();

		final dec = if(rdr.peek('.'.code) && rdr.peekNotAt(1, LOWER_U)) {
			rdr.next();

			if(rdr.peek(DIGIT)) {
				final dec = new Buffer();

				do {
					dec.addChar(rdr.eat());
				} while(rdr.peek(DIGIT));

				Some(dec.toString());
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
			final i = int.toString();
			return switch dec {
				case None: T_Int(span(), i, exp);
				case Some(d): T_Dec(span(), i, d, exp);
			}
		}
	}

	inline function readExponent() {
		final exp = new Buffer();
		final cur = rdr.peek();
		final ruleBegin = here();

		if(cur == '+'.code || cur == '-'.code) {
			exp.addChar(rdr.eat());
		}

		if(rdr.peek(DIGIT)) {
			do {
				exp.addChar(rdr.eat());
			} while(rdr.peek(DIGIT));

			return exp.toString();
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
		final name = new Buffer();

		do {
			name.addChar(rdr.eat());
		} while(rdr.peek(ALNUM_Q));

		final n = name.toString();

		return if(rdr.eat(':'.code)) {
			T_Label(span(), n);
		} else {
			T_Name(span(), n);
		}
	}

	inline function readPunned() {
		final punned = new Buffer();

		if(rdr.peek(LOWER_U)) {
			punned.addChar(rdr.eat());
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
			punned.addChar(rdr.eat());
		}

		return T_Punned(span(), punned.toString());
	}

	inline function readTypeName() {
		final name = new Buffer();

		do {
			name.addChar(rdr.eat());
		} while(rdr.peek(ALNUM_Q));

		final n = name.toString();

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
		final sym = new Buffer();

		while(rdr.peekNot('`'.code)) {
			sym.addChar(rdr.eat());
		}

		rdr.eat();

		return T_Litsym(span(), sym.toString());
	}

	inline function readTag() {
		final tag = new Buffer();

		while(rdr.peek(ALNUM)) {
			tag.addChar(rdr.eat());
		}

		return T_Tag(span(), tag.toString());
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
								Spanned({
									span: Span.at(end, source),
									message: 'Escape character `$c` ' + (
										if(c == '('.code) "is not allowed in char literals"
										else "does not exist"
									),
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
		final hex = new Buffer();

		hex.addString("0x");

		for(_ in 0...2) {
			if(rdr.peek(XDIGIT)) {
				hex.addChar(rdr.eat());
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
							span: new Span(end.advance(-hex.length), end, source),
							isSecondary: true
						})
					]
				});
			}
		}
		
		return Util.parseInt(hex.toString());
	}
	
	function readUniEsc(): Char {
		final uni = new Buffer();

		uni.addString("0x");

		for(_ in 0...4) {
			if(rdr.peek(XDIGIT)) {
				uni.addChar(rdr.eat());
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
							span: new Span(end.advance(-uni.length), end, source),
							isSecondary: true
						})
					]
				});
			}
		}

		return Util.parseInt(uni.toString());
	}
	
	function readOctEsc(): Char {
		final oct = new Buffer();

		for(_ in 0...3) {
			if(rdr.peek(ODIGIT)) {
				oct.addChar(rdr.eat());
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
							span: new Span(end.advance(-(oct.length + 2)), end, source),
							isSecondary: true
						})
					]
				});
			}
		}

		return Util.parseOctal(oct.toString());
	}

	inline function readStr() {
		var builder = new Buffer();
		final segments = [];

		while(rdr.hasNext()) switch rdr.eat() {
			case '"'.code:
				if(builder.length != 0) segments.push(SStr(builder.toString()));
				break;
			
			case '\\'.code:
				if(builder.length != 0) {
					segments.push(SStr(builder.toString()));
					builder = new Buffer();
				}

				final esc = rdr.eat();

				if(esc == '('.code) {
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

					segments.push(SCode(tokens.rev()));
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
										span: Span.at(end, source),
										message: 'Escape character `\\$c` does not exist',
										isPrimary: true
									}),
									Spanned({
										span: Span.at(end.advance(-1), source),
										isSecondary: true
									})
								]
							});
					};

					segments.push(SChar(char));
				}

			case c:
				builder.addChar(c);
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
			final nth = new Buffer();

			do {
				nth.addChar(rdr.eat());
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
				return T_AnonArg(span(), depth, Util.nonNull(Std.parseInt(nth.toString())));
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