package parsing;

import parsing.ast.decls.GenericParam;
import parsing.ast.Infix.Assignable;
import parsing.ast.Cascade;
import util.Buffer;
import text.Span;
import reporting.Diagnostic;
import reporting.DiagnosticInfo;
import lexing.Token;
import parsing.ParseResult;
import parsing.ast.Program;
import parsing.ast.Type;
import parsing.ast.Expr;
import parsing.ast.Message;
import parsing.ast.Stmt;
import parsing.ast.*;
import parsing.ast.decls.TypesSpec;
import parsing.ast.decls.Decl;
import parsing.ast.decls.Alias;
import parsing.ast.decls.Module;
import parsing.ast.decls.Class;
import parsing.ast.decls.Protocol;
import parsing.ast.decls.Category;
import parsing.ast.decls.Kind;
import parsing.ast.decls.Member;
import parsing.ast.decls.Case;
import parsing.ast.decls.Init;
import parsing.ast.decls.Method;
import parsing.ast.decls.Operator;
import parsing.ast.decls.Body;
import parsing.ast.decls.*;

/*
 * I should ideally never have to touch this piece of garbage ever again
 * once I finish script mode.
 */

class Parser {
	public static function parse(tokens: List<Token>) return trimTokens(tokens)._match(
		at([T_LSep(_), ...rest]) => parse(rest),
		at([T_Use(_1), T_Litsym(_2, "script"), ...rest]) => parseScript(_1, _2, rest),
		at(tokens) => parseModular(tokens)
	);

	static function parseScript(_1: Span, _2: Span, input: List<Token>): Program {
		final decls = [
			SDecl(DUse({
				generics: Nil,
				span: _1,
				kind: Pragma(_2, "script")
			}))
		];

		throw "NYI";
	}

	static function parseModular(input: List<Token>) {
		var tokens = input;
		var expectSep = false;
		var lastWasError = false;

		final errors = [];
		final decls = [];
		final badTokens = [];
		
		while(tokens != Nil) {
			final oldTokens = tokens;

			if(expectSep) {
				final first = tokens.head();
				
				tokens = tokens.tail();

				if(!isAnySep(first)) {
					errors.push(
						new Diagnostic({
							severity: reporting.Severity.ERROR,
							message: "Syntax error",
							info: [
								Spanned({
									span: first.span(),
									message: 'Unexpected ${first.basicTokenName()}, was expecting a comma or newline instead',
									isPrimary: true
								})
							]
						})
					);

					badTokens.push(first);
				}
			}

			switch nextDecl(Nil, tokens) {
				case Success(d, rest):
					decls.push(d);
					tokens = rest;
					if(!expectSep) expectSep = true;
					if(lastWasError) lastWasError = false;
				
				case Failure(begin, None) | Fatal(begin, None):
					final first = begin.head();

					if(!badTokens.contains(first) && !(lastWasError && isAnySep(first))) {
						errors.push(
							new Diagnostic({
								severity: reporting.Severity.ERROR,
								message: "Syntax error",
								info: [
									Spanned({
										span: first.span(),
										message: 'Unexpected ${first.basicTokenName()}',
										isPrimary: true
									})
								]
							})
						);

						badTokens.push(first);
					}
					
					if(tokens != Nil) tokens = tokens.tail(); // Causes weird bug if I remove this
					while(true) switch tokens {
						case Nil | Cons(T_LSep(_), _): break;
						case Cons(_, rest): tokens = rest;
					}

					expectSep = false;
					lastWasError = true;
		
				case Failure(begin, Some(end)) | Fatal(begin, Some(end)):
					final first = begin.head();
					final last = (end != Nil ? end : begin.rev()).head();
					
					if(!badTokens.contains(last)) {
						final info = [
							Spanned({
								span: last.span(),
								message: 'Unexpected ${last.basicTokenName()}',
								isPrimary: true
							})
						];

						if(first != last) {
							info.push(Spanned({
								span: first.span(),
								message: "Starting here",
								isSecondary: true
							}));
						}

						errors.push(
							new Diagnostic({
								severity: reporting.Severity.ERROR,
								message: "Syntax error",
								info: info
							})
						);

						badTokens.push(last);
					}
					
					while(true) switch tokens {
						case Nil | Cons(T_LSep(_), _): break;
						case Cons(_, rest): tokens = rest;
					}

					expectSep = false;
					lastWasError = true;

				case Eof(begin):
					final realBegin = (
						if(begin != Nil) begin
						else if(tokens != Nil) tokens
						else oldTokens
					);

					final first = realBegin.head();
					final last = realBegin.rev().head();

					if(!badTokens.contains(first)) {
						final info = [
							Spanned({
								span: last.span(),
								message: 'Unexpected end of file after ${last.basicTokenName()}',
								isPrimary: true
							})
						];

						// Don't add another message if they're the same token
						if(first != last) {
							info.push(Spanned({
								span: first.span(),
								message: "Starting here",
								isSecondary: true
							}));
						}

						errors.push(
							new Diagnostic({
								severity: reporting.Severity.ERROR,
								message: "Syntax error",
								info: info
							})
						);

						badTokens.push(first);
					}

					tokens = Nil;
				
				case FatalError(diag):
					errors.push(diag);

					while(true) switch tokens {
						case Nil | Cons(T_LSep(_), _): break;
						case Cons(_, rest): tokens = rest;
					}

					expectSep = false;
					lastWasError = true;
			}
		}

		return Modular(errors, decls);
	}


	/* DECLS */

	static function nextDeclBody(tokens: List<Token>) return tokens._match(
		at([T_LBrace(begin), T_RBrace(end), ...rest]) => Success({begin: begin, of: [], end: end}, rest),
		at([T_LBrace(begin), ...rest]) => {
			final decls = [];

			while(true) switch nextDecl(Nil, rest) {
				case Success(decl, rest2):
					decls.push(decl);
					
					rest2._match(
						at([T_RBrace(end), ...rest3]) => return Success({begin: begin, of: decls, end: end}, rest3),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([isAnySep(_) => true, ...rest3]) => rest = rest3,
						_ => return Fatal(tokens, Some(rest2))
					);
				
				case err: return cast err;
			}

			Fatal(tokens, Some(rest));
		},
		//at([]) => Eof(tokens),
		_ => Failure(tokens, None)
	);

	static function nextDecl(generics, tokens: List<Token>) return tokens._match(
		at([T_Type(_1), ...rest]) => switch parseGenericParam(_1, rest) {
			case Success(param, Cons(isAnySep(_) => true, rest2)): nextDecl(Cons(param, generics), rest2);
			case Success(_, rest2): Fatal(tokens, Some(rest2));
			case err: cast err;
		},
		at([T_Use(_1), T_Litsym(_2, sym), isAnySep(_) => true, ...rest]) => parseUsePragma(generics.rev(), _1, _2, sym, rest),
		at([T_Use(_1), ...rest]) => fatalIfBad(tokens, parseUseDecl(generics.rev(), _1, rest)),
		at([T_Alias(_1), ...rest]) => fatalIfBad(tokens, parseAliasDecl(generics.rev(), _1, rest)),
		at([T_Module(_1), ...rest]) => fatalIfBad(tokens, parseModuleDecl(generics.rev(), _1, rest)),
		at([T_Class(_1), ...rest]) => fatalIfBad(tokens, parseClassDecl(generics.rev(), _1, rest)),
		at([T_Protocol(_1), ...rest]) => fatalIfBad(tokens, parseProtocolDecl(generics.rev(), _1, rest)),
		at([T_Category(_1), ...rest]) => fatalIfBad(tokens, parseCategoryDecl(generics.rev(), _1, rest)),
		at([T_Kind(_1), ...rest]) => fatalIfBad(tokens, parseKindDecl(generics.rev(), _1, rest)),
		at([T_My(_1), ...rest]) => if(generics == Nil) {
			fatalIfBad(tokens, parseMemberDecl(_1, rest));
		} else {
			FatalError(new Diagnostic({
				severity: reporting.Severity.ERROR,
				message: "Invalid member",
				info: [
					Spanned({
						span: _1,
						message: "Members are not allowed to be generic",
						isPrimary: true
					})
				]
			}));
		},
		at([T_Has(_1), ...rest]) => if(generics == Nil) {
			fatalIfBad(tokens, parseCaseDecl(_1, rest));
		} else {
			FatalError(new Diagnostic({
				severity: reporting.Severity.ERROR,
				message: "Invalid case",
				info: [
					Spanned({
						span: _1,
						message: "Cases are not allowed to be generic",
						isPrimary: true
					})
				]
			}));
		},
		at([T_Init(_1), ...rest]) => fatalIfBad(tokens, parseInitDecl(generics.rev(), _1, rest)),
		at([T_On(_1), ...rest]) => fatalIfBad(tokens, parseMethodDecl(generics.rev(), _1, rest)),
		at([T_Operator(_1), ...rest]) => fatalIfBad(tokens, parseOperatorDecl(generics.rev(), _1, rest)),
		at([T_Deinit(_1), ...rest]) => if(generics == Nil) {
			fatalIfBad(tokens, parseDeinitDecl(_1, rest));
		} else {
			FatalError(new Diagnostic({
				severity: reporting.Severity.ERROR,
				message: "Invalid deinitializer",
				info: [
					Spanned({
						span: _1,
						message: "Deinitializers are not allowed to be generic",
						isPrimary: true
					})
				]
			}));
		},
		at([_, ..._]) => Fatal(tokens, None),
		at([]) => Eof(tokens)
	);


	static function parseGenericParam(_1, tokens): ParseResult<GenericParam> return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final parents = switch parseTypeParents(rest, true) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};
			
			final attrs = new Map<GenericParamAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Flags(_3), ...rest2]) => {
					attrs[GenericParamAttr.IsFlags] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Strong(_3), ...rest2]) => {
					attrs[GenericParamAttr.IsStrong] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Uncounted(_3), ...rest2]) => {
					attrs[GenericParamAttr.IsUncounted] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Native(_3), T_LBracket(begin), ...rest2]) => {
					final spec = [rest2._match(
						at([T_Label(_4, label), ...rest3]) => switch parseBasicExpr(rest3) {
							case Success(expr, rest4):
								rest2 = rest4;
								{label: new Ident(_4, label), expr: expr};
							case err: return cast err;
						},
						_ => return Fatal(tokens, Some(rest2))
					)];

					while(true) rest2._match(
						at([T_RBracket(end), ...rest3]) => {
							attrs[GenericParamAttr.IsNative(begin, spec, end)] = Span.range(_2, _3);
							rest = rest3;
							break;
						},
						at([isAnySep(_) => true, ...rest3] | rest3) => rest3._match(
							at([T_Label(_4, label), ...rest4]) => switch parseBasicExpr(rest4) {
								case Success(expr, rest5):
									rest2 = rest5;
									spec.push({label: new Ident(_4, label), expr: expr});
								case err: return cast err;
							},
							_ => return Fatal(tokens, Some(rest3))
						)
					);
				},
				_ => break
			);

			final rule = rest._match(
				at([T_If(_2), ...rest2]) => switch parseGenericRule(rest2) {
					case Success(rule, rest3):
						rest = rest3;
						Some({span: _2, rule: rule});
					case err: return cast err;
				},
				_ => None
			);

			final body = switch nextDeclBody(rest) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};

			Success({
				span: _1,
				name: name,
				params: params,
				parents: parents,
				attrs: attrs,
				rule: rule,
				body: body
			}, rest);

		case err: fatalIfBad(tokens, cast err);
	}

	static function parseGenericRule(tokens): ParseResult<GenericRule> return switch parseGenericRuleTerm(tokens) {
		case Success(left, rest): updateIfBad(rest, parseGenericRuleCond(left, rest));
		case err: updateIfBad(tokens, err);
	}

	static function parseGenericRuleCond(left, tokens: List<Token>): ParseResult<GenericRule> return tokens._match(
		at([T_AndAnd(_1), ...rest]) => switch parseGenericRuleTerm(rest) {
			case Success(right, rest2): parseGenericRuleCond(GenericRule.And(left, _1, right), rest2);
			case err: err;
		},
		at([T_BarBar(_1), ...rest]) => switch parseGenericRuleTerm(rest) {
			case Success(right, rest2): parseGenericRuleCond(GenericRule.Or(left, _1, right), rest2);
			case err: err;
		},
		at([T_CaretCaret(_1), ...rest]) => switch parseGenericRuleTerm(rest) {
			case Success(right, rest2): parseGenericRuleCond(GenericRule.Xor(left, _1, right), rest2);
			case err: err;
		},
		at([T_BangBang(_1), ...rest]) => switch parseGenericRuleTerm(rest) {
			case Success(right, rest2): parseGenericRuleCond(GenericRule.Nor(left, _1, right), rest2);
			case err: err;
		},
		_ => Success(left, tokens)
	);
	
	static function parseGenericRuleTerm(tokens: List<Token>) return tokens._match(
		at([T_LParen(_1), ...rest]) => parseGenericRuleParen(_1, rest),
		at([T_Bang(_1), T_LParen(_2), ...rest]) => switch parseGenericRuleParen(_2, rest) {
			case Success(rule, rest2): Success(GenericRule.Not(_1, rule), rest2);
			case err: err;
		},
		at([T_Bang(_1), ...rest]) => switch parseType(rest, true) {
			case Success(type, rest2): Success(GenericRule.Negate(_1, type), rest2);
			case err: cast err;
		},
		at([T_TypeName(_, _) | T_Wildcard(_), ..._]) => switch parseType(tokens, true) {
			case Success(type1, rest): rest._match(
				at([T_Question(_1), ...rest2]) => Success(GenericRule.Exists(type1, _1), rest2),
				
				at([T_QuestionEq(_1), ...rest2]) => switch parseType(rest2, true) {
					case Success(type2, rest3): {
						final chain = Cons2(_1, type2, Nil2);
						var curChain = chain;
						
						while(true) rest3._match(
							at([T_QuestionEq(_2), ...rest4]) => switch parseType(rest4, true) {
								case Success(type3, rest5):
									rest3 = rest5;
									curChain = Cons2(_2, type3, Nil2);
									util.EnumValues.setParameter(chain, 2, curChain);
								case err: return cast err;
							},
							_ => break
						);
						
						Success(GenericRule.Eq(type1, chain), rest3);
					}
					case err: cast err;
				},
				at([T_BangEq(_1), ...rest2]) => switch parseType(rest2, true) {
					case Success(type2, rest3): {
						final chain = Cons2(_1, type2, Nil2);
						var curChain = chain;
						
						while(true) rest3._match(
							at([T_BangEq(_2), ...rest4]) => switch parseType(rest4, true) {
								case Success(type3, rest5):
									rest3 = rest5;
									curChain = Cons2(_2, type3, Nil2);
									util.EnumValues.setParameter(chain, 2, curChain);
								case err: return cast err;
							},
							_ => break
						);
						
						Success(GenericRule.Ne(type1, chain), rest3);
					}
					case err: cast err;
				},
				at([T_Of(_1), ...rest2]) => switch parseType(rest2, true) {
					case Success(type2, rest3): {
						final chain = Cons2(_1, type2, Nil2);
						var curChain = chain;
						
						while(true) rest3._match(
							at([T_Of(_2), ...rest4]) => switch parseType(rest4, true) {
								case Success(type3, rest5):
									rest3 = rest5;
									curChain = Cons2(_2, type3, Nil2);
									util.EnumValues.setParameter(chain, 2, curChain);
								case err: return cast err;
							},
							_ => break
						);
						
						Success(GenericRule.Of(type1, chain), rest3);
					}
					case err: cast err;
				},
				
				at([op = T_Lt(_1) | T_LtEq(_1) | T_Gt(_1) | T_GtEq(_1), ...rest2]) => switch parseType(rest2, true) {
					case Success(type2, rest3): {
						inline function getOp(op: Token): GenericRule.CmpOp return switch op {
							case T_Lt(_): Lt;
							case T_LtEq(_): Le;
							case T_Gt(_): Gt;
							case T_GtEq(_): Ge;
							default: throw "error!";
						}
						
						final chain = Cons3(_1, getOp(op), type2, Nil3);
						var curChain = chain;
						
						while(true) rest3._match(
							at([op2 = T_Lt(_2) | T_LtEq(_2) | T_Gt(_2) | T_GtEq(_2), ...rest4]) => switch parseType(rest4, true) {
								case Success(type3, rest5):
									rest3 = rest5;
									curChain = Cons3(_2, getOp(op2), type3, Nil3);
									util.EnumValues.setParameter(chain, 3, curChain);
								case err: return cast err;
							},
							_ => break
						);
						
						Success(GenericRule.Cmp(type1, chain), rest3);
					}
					case err: cast err;
				},
				
				at([]) => Eof(tokens),
				_ => Failure(tokens, Some(rest))
			);
			case err: cast err;
		},
		at([]) => Eof(tokens),
		_ => Failure(tokens, None)
	);
	
	static function parseGenericRuleParen(_1, tokens: List<Token>) {
		if(tokens.head().match(T_RParen(_))) return Fatal(tokens, None);

		var rest = tokens;
		var inner = Nil;
		var level = 1;
		
		while(level > 0) rest._match(
			at([T_LSep(_), ...rest2]) => rest = rest2,
			at([token, ...rest2]) => {
				switch token {
					case T_LParen(_): level++;
					case T_RParen(_): if(level-- == 0) break;
					default:
				}
			
				inner = inner.prepend(token);
				rest = rest2;
			},
			at([]) => return Eof(tokens)
		);

		rest = inner.revAppend(rest);

		final oldRest = rest;
		final leadingOp = rest._match(
			at([T_AndAnd(_), ...rest2]) => {
				rest = rest2;
				Some(util.Enums.getIndex(GenericRule, And));
			},
			at([T_BarBar(_), ...rest2]) => {
				rest = rest2;
				Some(util.Enums.getIndex(GenericRule, Or));
			},
			at([T_CaretCaret(_), ...rest2]) => {
				rest = rest2;
				Some(util.Enums.getIndex(GenericRule, Xor));
			},
			at([T_BangBang(_), ...rest2]) => {
				rest = rest2;
				Some(util.Enums.getIndex(GenericRule, Nor));
			},
			_ => None
		);

		return switch [leadingOp, parseGenericRule(rest)] {
			case [Some(index), Success(rule, _)] if(rule.getIndex() != index): Fatal(tokens, Some(oldRest));
			case [_, Success(rule, Cons(T_RParen(_2), rest2))]: Success(GenericRule.Paren(_1, rule, _2), rest2);
			case [_, Success(_, rest2)]: Fatal(tokens, Some(rest2));
			case [_, err]: fatalIfBad(tokens, err);
		}
	}


	static function parseUsePragma(generics, _1, _2, sym, tokens) {
		return Success(DUse({generics: generics, span: _1, kind: Pragma(_2, sym)}), tokens);
	}
	
	static function parseUseDecl(generics, _1, tokens) return switch parseUseTree(tokens) {
		case Success(spec, rest):
			final from: Null<UseFrom> = rest._match(
				at([T_Label(_2, "from"), ...rest2]) => rest2._match(
					at([T_Str(_3, segs)]) => switch parseStrSegs(segs) {
						case Success([PStr(path)], rest3): rest = rest3; UFile(_2, _3, path);
						case Success(_, _): return Fatal(tokens, Some(rest2)); // TODO: custom error message
						case err: return fatalIfBad(tokens, cast err);
					},
					_ => switch parseType(rest2) {
						case Success(t, rest3): rest = rest3; UType(_2, t);
						case err: return fatalIfBad(tokens, cast err);
					}
				),
				_ => null
			);
			
			rest._match(
				at([T_Label(_2, "as"), ...rest2]) => switch parseUseTree(rest2) {
					case Success(tree, rest3): Success(DUse({generics: generics, span: _1, kind: Import(spec, from, new Tuple2(_2, tree))}), rest3);
					case err: fatalIfBad(tokens, cast err);
				},
				_ => Success(DUse({generics: generics, span: _1, kind: Import(spec, from)}), rest)
			);
		
		case err: fatalIfBad(tokens, cast err);
	};
	
	static function parseUseTree(tokens: List<Token>): ParseResult<UseTree> return tokens._match(
		at([T_HashLBracket(_), ...rest]) => {
			final types = [];
			
			while(true) switch parseType(rest) {
				case Success(t, rest2):
					types.push(t);
					
					rest2._match(
						at([T_RBracket(_), ...rest3]) => return Success(UTypes(types), rest3),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([isAnySep(_) => true, ...rest3]) => rest = rest3,
						_ => return Fatal(tokens, Some(rest2))
					);

				case err: return cast err;
			}

			Fatal(rest, None);
		},
		at([T_HashLParen(_), ...rest]) => {
			final pairs = [];
			
			while(true) switch parseType(rest) {
				case Success(t, rest2): rest2._match(
					at([T_EqGt(_1), ...rest3]) => switch parseUseTree(rest3) {
						case Success(tree, rest4):
							pairs.push(new Tuple3(t, _1, tree));
							
							rest4._match(
								at([T_RParen(_), ...rest5]) => return Success(UMap(pairs), rest5),
								at([] | [isAnySep(_) => true]) => return Eof(tokens),
								at([isAnySep(_) => true, ...rest5]) => rest = rest5,
								_ => return Fatal(tokens, Some(rest4))
							);
						
						case err: return cast err;
					},
					_ => Fatal(tokens, Some(rest2))
				);

				case err: return cast err;
			}

			Fatal(rest, None);
		},
		_ => switch parseType(tokens) {
			case Success(type, rest): Success(UType(type), rest);
			case err: cast err;
		}
	);


	static function parseAliasDecl(generics, _1, tokens) return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final attrs = new Map<AliasAttr, Span>();

			switch parseTypeAnno(rest) {
				case Success(type, rest2):
					while(true) rest2._match(
						at([T_Is(_2), T_Hidden(_3), ...rest3]) => switch parseType(rest3) {
							case Success(outer, rest4):
								attrs[AliasAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
								rest2 = rest4;
							case Failure(_, _):
								attrs[AliasAttr.IsHidden(None)] = Span.range(_2, _3);
								rest2 = rest3;
							case err: return cast err;
						},
						at([T_Is(_2), T_Friend(_3), ...rest3]) => switch parseTypeSpec(rest3) {
							case Success(spec, rest4):
								attrs[AliasAttr.IsFriend(spec)] = Span.range(_2, _3);
								rest2 = rest4;
							case err: return cast err;
						},
						at([T_Is(_2), T_Noinherit(_3), ...rest3]) => {
							attrs[AliasAttr.IsNoinherit] = Span.range(_2, _3);
							rest2 = rest3;
						},
						_ => break
					);

					final body = switch nextDeclBody(rest2) {
						case Success(made, rest3):
							rest2 = rest3;
							Some(made);
						case Failure(_, _): None;
						case err: return cast err;
					};

					Success(DAlias({
						generics: generics,
						span: _1,
						name: name,
						params: params,
						kind: Strong(type, body),
						attrs: attrs
					}), rest2);

				case Failure(_, _):
					while(true) rest._match(
						at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
							case Success(outer, rest3):
								attrs[AliasAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
								rest = rest3;
							case Failure(_, _):
								attrs[AliasAttr.IsHidden(None)] = Span.range(_2, _3);
								rest = rest2;
							case err: return cast err;
						},
						at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
							case Success(spec, rest3):
								attrs[AliasAttr.IsFriend(spec)] = Span.range(_2, _3);
								rest = rest3;
							case err: return cast err;
						},
						at([T_Is(_2), T_Noinherit(_3), ...rest2]) => {
							attrs[AliasAttr.IsNoinherit] = Span.range(_2, _3);
							rest = rest2;
						},
						_ => break
					);

					rest._match(
						at([T_Eq(_)]) => Eof(tokens),
						at([T_Eq(_2), ...rest2]) => switch parseType(rest2) {
							case Success(type, rest3): Success(DAlias({
								generics: generics,
								span: _1,
								name: name,
								params: params,
								kind: Direct(_2, type),
								attrs: attrs
							}), rest3);
							case err: cast err;
						},
						_ => {
							final body = switch nextDeclBody(rest) {
								case Success(block, rest2):
									rest = rest2;
									Some(block);
								case Failure(_, _): None;
								case err: return cast err;
							};

							Success(DAlias({
								generics: generics,
								span: _1,
								name: name,
								params: params,
								kind: Opaque(body),
								attrs: attrs
							}), rest);
						}
					);

				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}


	static function parseModuleDecl(generics, _1, tokens) return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final parents = switch parseTypeParents(rest) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return cast err;
			};
			
			final attrs = new Map<ModuleAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ModuleAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ModuleAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Sealed(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ModuleAttr.IsSealed(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ModuleAttr.IsSealed(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Main(_3), ...rest2]) => {
					attrs[ModuleAttr.IsMain] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
					case Success(spec, rest3):
						attrs[ModuleAttr.IsFriend(spec)] = Span.range(_2, _3);
						rest = rest3;
					case err: return cast err;
				},
				at([T_Is(_2), T_Native(_3), T_Litsym(_4, lib), ...rest2]) => {
					attrs[ModuleAttr.IsNative(_4, lib)] = Span.range(_2, _3);
					rest = rest2;
				},
				_ => break
			);

			switch nextDeclBody(rest) {
				case Success(body, rest2): Success(DModule({
					generics: generics,
					span: _1,
					name: name,
					params: params,
					parents: parents,
					attrs: attrs,
					body: body
				}), rest2);
				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}


	static function parseClassDecl(generics, _1, tokens) return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final parents = switch parseTypeParents(rest) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return cast err;
			};
			
			final attrs = new Map<ClassAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ClassAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ClassAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Sealed(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ClassAttr.IsSealed(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ClassAttr.IsSealed(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Strong(_3), ...rest2]) => {
					attrs[ClassAttr.IsStrong] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Uncounted(_3), ...rest2]) => {
					attrs[ClassAttr.IsUncounted] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
					case Success(spec, rest3):
						attrs[ClassAttr.IsFriend(spec)] = Span.range(_2, _3);
						rest = rest3;
					case err: return cast err;
				},
				at([T_Is(_2), T_Native(_3), T_LBracket(begin), ...rest2]) => {
					final spec = [rest2._match(
						at([T_Label(_4, label), ...rest3]) => switch parseBasicExpr(rest3) {
							case Success(expr, rest4):
								rest2 = rest4;
								{label: new Ident(_4, label), expr: expr};
							case err: return cast err;
						},
						_ => return Fatal(tokens, Some(rest2))
					)];

					while(true) rest2._match(
						at([T_RBracket(end), ...rest3]) => {
							attrs[ClassAttr.IsNative(begin, spec, end)] = Span.range(_2, _3);
							rest = rest3;
							break;
						},
						at([isAnySep(_) => true, ...rest3] | rest3) => rest3._match(
							at([T_Label(_4, label), ...rest4]) => switch parseBasicExpr(rest4) {
								case Success(expr, rest5):
									rest2 = rest5;
									spec.push({label: new Ident(_4, label), expr: expr});
								case err: return cast err;
							},
							_ => return Fatal(tokens, Some(rest3))
						)
					);
				},
				_ => break
			);

			switch nextDeclBody(rest) {
				case Success(body, rest2): Success(DClass({
					generics: generics,
					span: _1,
					name: name,
					params: params,
					parents: parents,
					attrs: attrs,
					body: body
				}), rest2);
				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}


	static function parseProtocolDecl(generics, _1, tokens) return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final parents = switch parseTypeParents(rest) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return cast err;
			};
			
			final attrs = new Map<ProtocolAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ProtocolAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ProtocolAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Sealed(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[ProtocolAttr.IsSealed(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[ProtocolAttr.IsSealed(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
					case Success(spec, rest3):
						attrs[ProtocolAttr.IsFriend(spec)] = Span.range(_2, _3);
						rest = rest3;
					case err: return cast err;
				},
				_ => break
			);

			switch nextDeclBody(rest) {
				case Success(body, rest2): Success(DProtocol({
					generics: generics,
					span: _1,
					name: name,
					params: params,
					parents: parents,
					attrs: attrs,
					body: body
				}), rest2);
				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}


	static function parseCategoryDecl(generics, _1, tokens) return switch parseType(tokens) {
		case Success(path, rest):
			final type = rest._match(
				at([T_For(_), ...rest2]) => switch parseType(rest2) {
					case Success(made, rest3):
						rest = rest3;
						Some(made);
					case err: return cast err;
				},
				_ => None
			);
			
			final attrs = new Map<CategoryAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[CategoryAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[CategoryAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
					case Success(spec, rest3):
						attrs[CategoryAttr.IsFriend(spec)] = Span.range(_2, _3);
						rest = rest3;
					case err: return cast err;
				},
				_ => break
			);

			switch nextDeclBody(rest) {
				case Success(body, rest2): Success(DCategory({
					generics: generics,
					span: _1,
					path: path,
					type: type,
					attrs: attrs,
					body: body
				}), rest2);
				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}


	static function parseKindDecl(generics, _1, tokens) return switch parseTypeDeclName(tokens) {
		case Success({name: name, params: params}, rest):
			final repr = switch parseTypeAnno(rest) {
				case Success(type, rest2):
					rest = rest2;
					Some(type);
				case Failure(_, _): None;
				case err: return cast err;
			};

			final parents = switch parseTypeParents(rest) {
				case Success(made, rest2):
					rest = rest2;
					Some(made);
				case Failure(_, _): None;
				case err: return cast err;
			};
			
			final attrs = new Map<KindAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[KindAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[KindAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Sealed(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[KindAttr.IsSealed(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[KindAttr.IsSealed(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Strong(_3), ...rest2]) => {
					attrs[KindAttr.IsStrong] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Uncounted(_3), ...rest2]) => {
					attrs[KindAttr.IsUncounted] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Flags(_3), ...rest2]) => {
					attrs[KindAttr.IsFlags] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Friend(_3), ...rest2]) => switch parseTypeSpec(rest2) {
					case Success(spec, rest3):
						attrs[KindAttr.IsFriend(spec)] = Span.range(_2, _3);
						rest = rest3;
					case err: return cast err;
				},
				_ => break
			);

			switch nextDeclBody(rest) {
				case Success(body, rest2): Success(DKind({
					generics: generics,
					span: _1,
					name: name,
					params: params,
					repr: repr,
					parents: parents,
					attrs: attrs,
					body: body
				}), rest2);
				case err: cast err;
			}

		case err: fatalIfBad(tokens, cast err);
	}
	

	static function parseMemberDecl(_1, tokens: List<Token>) return tokens._match(
		at([T_Name(_2, name), ...rest]) => {
			final type = switch parseTypeAnno(rest) {
				case Success(of, rest2):
					rest = rest2;
					Some(of);
				case Failure(_, _): None;
				case err: return cast err;
			};

			final attrs = new Map<MemberAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Static(_3), ...rest2]) => {
					attrs[MemberAttr.IsStatic] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[MemberAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[MemberAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return cast err;
				},
				at([T_Is(_2), T_Readonly(_3), ...rest2]) => {
					attrs[MemberAttr.IsReadonly] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Getter(_3), T_Litsym(_4, sym), ...rest2]) => {
					attrs[MemberAttr.IsGetter(Some({span: _4, name: sym}))] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Getter(_3), ...rest2]) => {
					attrs[MemberAttr.IsGetter(None)] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Setter(_3), T_Litsym(_4, sym), ...rest2]) => {
					attrs[MemberAttr.IsSetter(Some({span: _4, name: sym}))] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Setter(_3), ...rest2]) => {
					attrs[MemberAttr.IsSetter(None)] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Noinherit(_3), ...rest2]) => {
					attrs[MemberAttr.IsNoinherit] = Span.range(_2, _3);
					rest = rest2;
				},
				_ => break
			);

			final value = rest._match(
				at([T_Eq(_), ...rest2]) => switch parseFullExpr(rest2) {
					case Success(expr, rest3):
						rest = rest3;
						Some(expr);
					case err: return cast err;
				},
				_ => None
			);

			Success(DMember({
				span: _1,
				name: {span: _2, name: name},
				type: type,
				attrs: attrs,
				value: value
			}), rest);

		},
		at([]) => Eof(tokens),
		_ => Fatal(tokens, None)
	);

	
	static function parseCaseDecl(_1, tokens: List<Token>) return tokens._match(
		at([T_Name(_2, name), ...rest]) => {
			final value = rest._match(
				at([T_EqGt(_), ...rest2]) => switch parseExpr(rest2) {
					case Success(expr, rest3):
						rest = rest3;
						Some(expr);
					case err: return cast err;
				},
				_ => None
			);

			final init = switch parseBlock(rest) {
				case Success(block, rest2):
					rest = rest2;
					Some(block);
				case Failure(_, _): None;
				case err: return cast err;
			};

			Success(DCase({
				span: _1,
				kind: Scalar({span: _2, name: name}, value),
				init: init
			}), rest);
		},

		at([T_LBracket(begin), ...rest = [T_Label(_, _), ..._]]) => {
			final params = [rest._match(
				// Checking for `a: (B)` syntax before `a: a' (B)` syntax is
				// probably less expensive than doing it after.
				at([T_Label(_2, label), ...rest2 = [T_LParen(_), ..._]]) => switch parseTypeAnno(rest2) {
					case Success(type, rest3):
						rest = rest3;
						{label: Some(new Ident(_2, label)), name: None, type: type};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([T_Label(_2, label), _.asSoftName() => T_Name(_3, name), ...rest2]) => switch parseTypeAnno(rest2) {
					case Success(type, rest3):
						rest = rest3;
						{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([T_Label(_, _), ...rest2]) => return Fatal(tokens, Some(rest2)),
				at([]) => return Eof(tokens),
				_ => return Fatal(tokens, Some(rest))
			)];

			while(true) {
				rest._match(
					at([T_RBracket(end), ...rest2]) => {
						final assoc = rest2._match(
							at([T_EqGt(_), T_LBracket(_), ...rest3]) => switch finishTypeMsg(rest3) {
								case Success(msg, rest4):
									rest2 = rest4;
									Some(msg.msg);
								case err: return fatalIfBad(rest2, cast err);
							},
							at([T_EqGt(_), ...rest3]) => return Fatal(rest2, Some(rest3)),
							_ => None
						);
						final init = switch parseBlock(rest2) {
							case Success(block, rest3):
								rest2 = rest3;
								Some(block);
							case Failure(_, _): None;
							case err: return cast err;
						};
						
						return Success(DCase({
							span: _1,
							kind: Tagged({begin: begin, of: Multi(params), end: end}, assoc),
							init: init
						}), rest2);
					},
					at([isAnySep(_) => true, ...rest2]) => rest = rest2,
					at([T_Label(_, _), ..._]) => {},
					at([]) => return Eof(tokens),
					_ => return Fatal(tokens, Some(rest))
				);

				params.push(rest._match(
					at([T_Label(_2, label), ...rest2 = [T_LParen(_), ..._]]) => switch parseTypeAnno(rest2) {
						case Success(type, rest3):
							rest = rest3;
							{label: Some(new Ident(_2, label)), name: None, type: type};
						case err: return fatalIfBad(tokens, cast err);
					},
					at([T_Label(_2, label), _.asSoftName() => T_Name(_3, name), ...rest2]) => switch parseTypeAnno(rest2) {
						case Success(type, rest3):
							rest = rest3;
							{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type};
						case err: return fatalIfBad(tokens, cast err);
					},
					at([T_Label(_, _), ...rest2]) => return Fatal(tokens, Some(rest2)),
					at([_.asSoftName() => T_Name(_2, name), ...rest2]) => switch parseTypeAnno(rest2) {
						case Success(type, rest3):
							rest = rest3;
							{label: None, name: Some(new Ident(_2, name)), type: type};
						case err: return fatalIfBad(tokens, cast err);
					},
					at([]) => return Eof(tokens),
					_ => switch parseTypeAnno(rest) {
						case Success(type, rest3):
							rest = rest3;
							{label: None, name: None, type: type};
						case err: return fatalIfBad(tokens, cast err);
					}
				));
			};

			return Fatal(tokens, Some(rest));
		},
		at([T_LBracket(begin), _.asAnyName() => T_Name(_2, name), T_RBracket(end), ...rest]) => {
			final assoc = rest._match(
				at([T_EqGt(_), T_LBracket(_), ...rest2]) => switch finishTypeMsg(rest2) {
					case Success(msg, rest3):
						rest = rest3;
						Some(msg.msg);
					case err: return fatalIfBad(rest, cast err);
				},
				at([T_EqGt(_), ...rest2]) => return Fatal(rest, Some(rest2)),
				_ => None
			);
			final kind = CaseKind.Tagged({begin: begin, of: Single({span: _2, name: name}), end: end}, assoc);
			final init = switch parseBlock(rest) {
				case Success(block, rest2):
					rest = rest2;
					Some(block);
				case Failure(_, _): None;
				case err: return cast err;
			};

			Success(DCase({
				span: _1,
				kind: kind,
				init: init
			}), rest);
		},

		at([]) => Eof(tokens),
		_ => Fatal(tokens, None)
	);


	/* SIGS */

	static function parseMultiSig(tokens: List<Token>): ParseResult<{params: Array<MultiParam>, end: Span}> {
		var rest = tokens;
		final params = [rest._match(
			// Checking for `a: (B)` syntax before `a: a' (B)` syntax is
			// probably less expensive than doing it after.
			at([T_Label(_2, label), ...rest2 = [T_LParen(_), ..._]]) => switch parseTypeAnno(rest2, true) {
				case Success(type, Cons(T_Eq(_), rest3)): switch parseExpr(rest3) {
					case Success(expr, rest4):
						rest = rest4;
						{label: Some(new Ident(_2, label)), name: None, type: type, value: Some(expr)};
					case err: return fatalIfFailed(cast err);
				}
				case Success(type, rest3):
					rest = rest3;
					{label: Some(new Ident(_2, label)), name: None, type: type, value: None};
				case err: return fatalIfBad(tokens, cast err);
			},
			at([T_Label(_2, label), _.asSoftName() => T_Name(_3, name), ...rest2]) => switch parseTypeAnno(rest2, true) {
				case Success(type, Cons(T_Eq(_), rest3)): switch parseExpr(rest3) {
					case Success(expr, rest4):
						rest = rest4;
						{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type, value: Some(expr)};
					case err: return fatalIfFailed(cast err);
				}
				case Success(type, rest3):
					rest = rest3;
					{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type, value: None};
				case err: return fatalIfBad(tokens, cast err);
			},
			at([T_Label(_, _), ...rest2]) => return Fatal(tokens, Some(rest2)),
			at([]) => return Eof(tokens),
			_ => return Fatal(tokens, Some(rest))
		)];

		while(true) {
			rest._match(
				at([T_RBracket(end), ...rest2]) => return Success({params: params, end: end}, rest2),
				at([isAnySep(_) => true, ...rest2]) => rest = rest2,
				at([T_Label(_, _), ..._]) => {},
				at([]) => return Eof(tokens),
				_ => return Fatal(tokens, Some(rest))
			);

			params.push(rest._match(
				at([T_Label(_2, label), ...rest2 = [T_LParen(_), ..._]]) => switch parseTypeAnno(rest2, true) {
					case Success(type, Cons(T_Eq(_), rest3)): switch parseExpr(rest3) {
						case Success(expr, rest4):
							rest = rest4;
							{label: Some(new Ident(_2, label)), name: None, type: type, value: Some(expr)};
						case err: return fatalIfFailed(cast err);
					}
					case Success(type, rest3):
						rest = rest3;
						{label: Some(new Ident(_2, label)), name: None, type: type, value: None};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([T_Label(_2, label), _.asSoftName() => T_Name(_3, name), ...rest2]) => switch parseTypeAnno(rest2, true) {
					case Success(type, Cons(T_Eq(_), rest3)): switch parseExpr(rest3) {
						case Success(expr, rest4):
							rest = rest4;
							{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type, value: Some(expr)};
						case err: return fatalIfFailed(cast err);
					}
					case Success(type, rest3):
						rest = rest3;
						{label: Some(new Ident(_2, label)), name: Some(new Ident(_3, name)), type: type, value: None};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([T_Label(_, _), ...rest2]) => return Fatal(tokens, Some(rest2)),
				at([T_LParen(_), ..._]) => switch parseTypeAnno(rest, true) {
					case Success(type, rest2):
						rest = rest2;
						{label: None, name: None, type: type, value: None};
					case err: return fatalIfFailed(cast err);
				},
				at([_.asSoftName() => T_Name(_2, name), ...rest2]) => switch parseTypeAnno(rest2, true) {
					case Success(type, Cons(T_Eq(_), rest3)): switch parseExpr(rest3) {
						case Success(expr, rest4):
							rest = rest4;
							{label: None, name: Some(new Ident(_2, name)), type: type, value: Some(expr)};
						case err: return fatalIfFailed(cast err);
					}
					case Success(type, rest3):
						rest = rest3;
						{label: None, name: Some(new Ident(_2, name)), type: type, value: None};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([]) => return Eof(tokens),
				_ => return Fatal(tokens, Some(rest))
			));
		};

		return Fatal(tokens, Some(rest));
	}

	
	/* METHODS */

	static function parseMethodDecl(generics, _1, tokens: List<Token>) return tokens._match(
		at([T_LBracket(begin), ...rest]) => {
			final result = rest._match(
				at([T_Label(_, _), ..._]) => switch parseMultiSig(rest) {
					case Success({params: params, end: end}, rest2):
						rest = rest2;
						{kind: MethodKind.Multi(params), end: end};
					case err: return fatalIfBad(rest, cast err);
				},
				at([_.asAnyName() => T_Name(_1, name), T_RBracket(end), ...rest2]) => {
					rest = rest2;
					{kind: MethodKind.Single({span: _1, name: name}), end: end};
				},
				_ => switch parseType(rest) {
					case Success(type, Cons(T_RBracket(end), rest2)):
						rest = rest2;
						{kind: MethodKind.Cast(type), end: end};
					case Success(_, rest2): return Fatal(rest, Some(rest2));
					case err: return fatalIfBad(rest, cast err);
				}
			);
			final kind = result.kind;
			final end = result.end;
			final ret = switch parseTypeAnno(rest) {
				case Success(type, rest2):
					rest = rest2;
					Some(type);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};
			final attrs = new Map<MethodAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Static(_3), ...rest2]) => {
					attrs[MethodAttr.IsStatic] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[MethodAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[MethodAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return fatalIfBad(rest2, cast err);
				},
				at([T_Is(_2), T_Main(_3), ...rest2]) => {
					attrs[MethodAttr.IsMain] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Getter(_3), ...rest2]) => {
					attrs[MethodAttr.IsGetter] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Setter(_3), ...rest2]) => {
					attrs[MethodAttr.IsSetter] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Noinherit(_3), ...rest2]) => {
					attrs[MethodAttr.IsNoinherit] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Unordered(_3), ...rest2]) => {
					attrs[MethodAttr.IsUnordered] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Native(_3), T_Litsym(_4, sym), ...rest2]) => {
					attrs[MethodAttr.IsNative(Some({span: _4, name: sym}))] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Native(_3), ...rest2]) => {
					attrs[MethodAttr.IsNative(None)] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Inline(_3), ...rest2]) => {
					attrs[MethodAttr.IsInline] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Asm(_3), ...rest2]) => {
					attrs[MethodAttr.IsAsm] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Macro(_3), ...rest2]) => {
					attrs[MethodAttr.IsMacro] = Span.range(_2, _3);
					rest = rest2;
				},
				_ => break
			);

			final body = switch parseBody(rest) {
				case Success(body2, rest2):
					rest = rest2;
					Some(body2);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};

			Success(DMethod({
				generics: generics,
				span: _1,
				spec: {begin: begin, of: kind, end: end},
				ret: ret,
				attrs: attrs,
				body: body
			}), rest);
		},
		_ => Failure(tokens, None)
	);


	/* INITS */

	static function parseInitDecl(generics, _1, tokens: List<Token>) return tokens._match(
		at([T_LBracket(begin), ...rest]) => {
			final result = rest._match(
				at([T_Label(_, _), ..._]) => switch parseMultiSig(rest) {
					case Success({params: params, end: end}, rest2):
						rest = rest2;
						{kind: InitKind.Multi(params), end: end};
					case err: return fatalIfBad(rest, cast err);
				},
				at([_.asAnyName() => T_Name(_1, name), T_RBracket(end), ...rest2]) => {
					rest = rest2;
					{kind: InitKind.Single({span: _1, name: name}), end: end};
				},
				_ => return Fatal(tokens, Some(rest))
			);
			final kind = result.kind;
			final end = result.end;
			final attrs = new Map<InitAttr, Span>();

			while(true) rest._match(
				at([T_Is(_2), T_Hidden(_3), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[InitAttr.IsHidden(Some(outer))] = Span.range(_2, _3);
						rest = rest3;
					case Failure(_, _):
						attrs[InitAttr.IsHidden(None)] = Span.range(_2, _3);
						rest = rest2;
					case err: return fatalIfBad(rest2, cast err);
				},
				at([T_Is(_2), T_Noinherit(_3), ...rest2]) => {
					attrs[InitAttr.IsNoinherit] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Unordered(_3), ...rest2]) => {
					attrs[InitAttr.IsUnordered] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Native(_3), T_Litsym(_4, sym), ...rest2]) => {
					attrs[InitAttr.IsNative(Some({span: _4, name: sym}))] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Native(_3), ...rest2]) => {
					attrs[InitAttr.IsNative(None)] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Asm(_3), ...rest2]) => {
					attrs[InitAttr.IsAsm] = Span.range(_2, _3);
					rest = rest2;
				},
				at([T_Is(_2), T_Macro(_3), ...rest2]) => {
					attrs[InitAttr.IsMacro] = Span.range(_2, _3);
					rest = rest2;
				},
				_ => break
			);

			final body = switch parseBody(rest) {
				case Success(body2, rest2):
					rest = rest2;
					Some(body2);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};

			Success(DInit({
				generics: generics,
				span: _1,
				spec: {begin: begin, of: kind, end: end},
				attrs: attrs,
				body: body
			}), rest);
		},
		at([T_Is(_2), T_Static(_3), ...rest]) => switch parseBody(rest) {
			case Success(body, rest2): Success(DDefaultInit({
				span: _1,
				attrs: [IsStatic => Span.range(_2, _3)],
				body: body
			}), rest2);
			case err: fatalIfBad(rest, cast err);
		},
		_ => switch parseBody(tokens) {
			case Success(body, rest): Success(DDefaultInit({
				span: _1,
				attrs: [],
				body: body
			}), rest);
			case err: fatalIfBad(tokens, cast err);
		}
	);


	/* OPERATORS */

	static function parseOperatorDecl(generics, _1, tokens: List<Token>) return tokens._match(
		at([T_Litsym(_2, sym), ...rest]) => {
			final spec = rest._match(
				at([T_LBracket(_), T_RBracket(_), ..._]) => return Fatal(tokens, Some(rest)), // TODO: custom error message here
				at([T_LBracket(begin), _.asSoftName() => T_Name(_3, name), ...rest2]) => switch parseTypeAnno(rest2) {
					case Success(type, Cons(T_RBracket(end), rest3)):
						rest = rest3;
						Some({begin: begin, of: {name: new Ident(_3, name), type: type}, end: end});
					case Success(_, rest3): return Fatal(tokens, Some(rest3));
					case err: return fatalIfBad(rest2, cast err);
				},
				_ => None
			);
			final ret = switch parseTypeAnno(rest) {
				case Success(type, rest2):
					rest = rest2;
					Some(type);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};
			final attrs = new Map<OperatorAttr, Span>();

			while(true) rest._match(
				at([T_Is(_3), T_Hidden(_4), ...rest2]) => switch parseType(rest2) {
					case Success(outer, rest3):
						attrs[OperatorAttr.IsHidden(Some(outer))] = Span.range(_3, _4);
						rest = rest3;
					case Failure(_, _):
						attrs[OperatorAttr.IsHidden(None)] = Span.range(_3, _4);
						rest = rest2;
					case err: return fatalIfBad(rest2, cast err);
				},
				at([T_Is(_3), T_Noinherit(_4), ...rest2]) => {
					attrs[OperatorAttr.IsNoinherit] = Span.range(_3, _4);
					rest = rest2;
				},
				at([T_Is(_3), T_Native(_4), T_Litsym(_5, sym), ...rest2]) => {
					attrs[OperatorAttr.IsNative(Some({span: _5, name: sym}))] = Span.range(_3, _4);
					rest = rest2;
				},
				at([T_Is(_3), T_Native(_4), ...rest2]) => {
					attrs[OperatorAttr.IsNative(None)] = Span.range(_3, _4);
					rest = rest2;
				},
				at([T_Is(_3), T_Inline(_4), ...rest2]) => {
					attrs[OperatorAttr.IsInline] = Span.range(_3, _4);
					rest = rest2;
				},
				at([T_Is(_3), T_Asm(_4), ...rest2]) => {
					attrs[OperatorAttr.IsAsm] = Span.range(_3, _4);
					rest = rest2;
				},
				at([T_Is(_3), T_Macro(_4), ...rest2]) => {
					attrs[OperatorAttr.IsMacro] = Span.range(_3, _4);
					rest = rest2;
				},
				_ => break
			);
			
			final body = switch parseBody(rest) {
				case Success(body2, rest2):
					rest = rest2;
					Some(body2);
				case Failure(_, _): None;
				case err: return fatalIfBad(rest, cast err);
			};

			Success(DOperator({
				generics: generics,
				span: _1,
				symbolSpan: _2,
				symbol: sym,
				spec: spec,
				ret: ret,
				attrs: attrs,
				body: body
			}), rest);
		},
		_ => Failure(tokens, None)
	);


	/* DEINITS */

	static function parseDeinitDecl(_1, tokens: List<Token>) return tokens._match(
		at([T_Is(_2), T_Static(_3), ...rest]) => switch parseBody(rest) {
			case Success(body, rest2): Success(DDeinit({
				span: _1,
				attrs: [IsStatic => Span.range(_2, _3)],
				body: body
			}), rest2);
			case err: fatalIfBad(rest, cast err);
		},
		_ => switch parseBody(tokens) {
			case Success(body, rest): Success(DDeinit({
				span: _1,
				attrs: [],
				body: body
			}), rest);
			case err: fatalIfBad(tokens, cast err);
		}
	);


	/* TYPES */

	static function parseTypeParents(tokens: List<Token>, allowEOL = false) return tokens._match(
		at([T_Of(_1), ...rest]) => switch parseType(rest) {
			case Success(type, rest2):
				final parents = [type];

				while(true) rest2._match(
					at([T_Comma(_), ...rest3]) => switch parseType(rest3) {
						case Success(type, rest4):
							parents.push(type);
							rest2 = rest4;
						case _ if(allowEOL): break;
						case err: return fatalIfBad(tokens, cast err);
					},
					_ => break
				);

				Success({span: _1, parents: parents}, rest2);
			
			case err: fatalIfBad(rest, cast err);
		},
		_ => Failure(tokens, None)
	);

	static function parseTypeDeclName(tokens: List<Token>): ParseResult<{name: Ident, params: Option<TypeParams>}> return tokens._match(
		at([T_TypeName(_1, name), ...rest]) => switch parseTypeArgs(rest) {
			case Success(params, rest2): Success({name: {span: _1, name: name}, params: Some(params)}, rest2);
			case Failure(_, _): Success({name: {span: _1, name: name}, params: None}, rest);
			case err: cast err;
		},
		_ => Failure(tokens, None)
	);

	static function parseTypeSpec(tokens: List<Token>) return tokens._match(
		at([T_HashLBracket(begin), ...rest]) => {
			final types = [];
			
			while(true) switch parseType(rest) {
				case Success(t, rest2):
					types.push(t);
					
					rest2._match(
						at([T_RBracket(end), ...rest3]) => return Success(Many(begin, types, end), rest3),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([isAnySep(_) => true, ...rest3]) => rest = rest3,
						_ => return Fatal(tokens, Some(rest2))
					);

				case err: return cast err;
			}

			Fatal(rest, None);
		},
		_ => switch parseType(tokens) {
			case Success(type, rest): Success(One(type), rest);
			case err: cast err;
		}
	);

	static function parseType(tokens: List<Token>, allowSingleWildcard = false): ParseResult<Type> {
		var rest = tokens;
		var leading = rest._match(
			at([T_Wildcard(_1), ...rest2]) => switch parseTypeArgs(rest2) {
				case Success(args, rest3): if(allowSingleWildcard) {
					return Success(TBlankParams(_1, args), rest3);
				} else {
					return Failure(tokens, Some(rest2));
				}
				case Failure(_, _): rest2._match(
					at([T_Dot(_), ...rest3 = [T_TypeName(_, _), ..._]]) => {
						rest = rest3;
						List.of(_1);
					},
					at([T_Dot(_), T_Wildcard(_), ..._]) => List.of(_1),
					_ => if(allowSingleWildcard) {
						return Success(TBlank(_1), rest2);
					} else {
						return Failure(tokens, None);
					}
				);
				case err: return cast err;
			},
			_ => Nil
		);
		
		if(leading != Nil) while(true) rest._match(
			at([T_Wildcard(_1), T_Dot(_), ...rest2]) => {
				leading = leading.prepend(_1);
				rest = rest2;
			},
			at([T_Wildcard(_), ..._]) => return Failure(tokens, Some(rest)),
			_ => {
				leading = leading.rev();
				break;
			}
		);
		
		return switch parseTypeSeg(rest) {
			case Success(first, rest2): rest2._match(
				at([T_Dot(_), T_TypeName(_, _), ..._]) => switch parseTypeSegs(rest2) {
					case Success(segs, rest3): Success(TSegs(leading, segs.prepend(first)), rest3);
					case Failure(_, _): Success(TSegs(leading, List.of(first)), rest2);
					case err: cast err;
				},
				_ => Success(TSegs(leading, List.of(first)), rest2)
			);
			case err: cast err;
		}
	}

	static function parseTypeSeg(tokens: List<Token>) return tokens._match(
		at([T_TypeName(_1, name), ...rest]) => switch parseTypeArgs(rest) {
			case Success(args, rest2): Success(TypeSeg.NameParams(_1, name, args), rest2);
			case Failure(_, _): Success(TypeSeg.Name(_1, name), rest);
			case err: cast err;
		},
		_ => Failure(tokens, None)
	);

	static function parseTypeSegs(tokens: List<Token>): ParseResult<List<TypeSeg>> return tokens._match(
		at([T_Dot(_), ...rest]) => switch parseTypeSeg(rest) {
			case Success(seg, rest2): switch parseTypeSegs(rest2) {
				case Success(segs, rest3): Success(Cons(seg, segs), rest3);
				case Failure(_, _): Success(List.of(seg), rest2);
				case err: err;
			}
			case err: cast err;
		},
		_ => Failure(tokens, None)
	);

	static function parseTypeArgs(tokens: List<Token>) return tokens._match(
		at([T_LBracket(begin), ...rest]) => {
			final types = [];
			
			while(true) switch parseType(rest, true) {
				case Success(t, rest2):
					types.push(t);

					rest2._match(
						at([T_RBracket(end), ...rest3]) => return Success({begin: begin, of: types, end: end}, rest3),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([T_LSep(_), _.asAnyName() => (T_Name(_, _) | T_Label(_, _) | T_Punned(_, _)), ..._], when(types.length == 1)) => return Failure(tokens, Some(rest2)),
						at([isAnySep(_) => true, ...rest3]) => rest = rest3,
						at([_.asAnyName() => (T_Name(_, _) | T_Label(_, _) | T_Punned(_, _)), ..._], when(types.length == 1)) => return Failure(tokens, Some(rest2)),
						_ => return Fatal(tokens, Some(rest2))
					);
				
				case err: return cast err;
			}

			Fatal(rest, None);
		},
		_ => Failure(tokens, None)
	);

	static function parseTypeAnno(tokens: List<Token>, allowSingleWildcard = false) return tokens._match(
		at([T_LParen(_)]) => Eof(tokens),
		at([T_LParen(_), ...rest]) => switch parseType(rest, allowSingleWildcard) {
			case Success(type, Cons(T_RParen(_), rest2)): Success(type, rest2);
			case Success(_, Nil): Eof(tokens);
			case Success(_, rest2): Fatal(tokens, Some(rest2));
			case err: err;
		},
		_ => Failure(tokens, None)
	);


	/* STATEMENTS */
	
	// TODO: enforce short arrow stmt
	static function parseBody(tokens: List<Token>): ParseResult<Body> return tokens._match(
		at([T_EqGt(_1), ...rest]) => switch parseStmt(rest) {
			case Success(stmt, rest2): Success(BArrow(_1, stmt), rest2);
			case err: fatalIfFailed(cast err);
		},
		_ => switch parseBlock(tokens) {
			case Success(block, rest): Success(BBlock(block), rest);
			case err: cast err;
		}
	);

	static function parseBlock(tokens: List<Token>): ParseResult<Block> return tokens._match(
		at([T_LBrace(begin), T_RBrace(end), ...rest]) => Success({begin: begin, stmts: [], end: end}, rest),
		at([T_LBrace(begin), ...rest]) => {
			final stmts = [];

			while(true) switch parseStmt(rest) {
				case Success(stmt, rest2):
					stmts.push(stmt);
					
					rest2._match(
						at([T_RBrace(end), ...rest3]) => return Success({begin: begin, stmts: stmts, end: end}, rest3),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([isAnySep(_) => true, ...rest3]) => rest = rest3,
						_ => return Fatal(rest, Some(rest2))
					);
				
				case err: return fatalIfBad(rest, cast err);
			}

			Fatal(tokens, Some(rest));
		},
		_ => Failure(tokens, None)
	);

	
	static function parseStmt(tokens: List<Token>): ParseResult<Stmt> return tokens._match(
		at([T_My(_1), _.asSoftName() => T_Name(_2, name), ...rest]) => {
			final type = switch parseTypeAnno(rest) {
				case Success(t, rest2):
					rest = rest2;
					Some(t);
				case Failure(_, _): None;
				case err: return cast err;
			};
			final expr = rest._match(
				at([T_Eq(_), ...rest2]) => switch parseFullExpr(rest2) {
					case Success(e, rest3):
						rest = rest3;
						Some(e);
					case err: return fatalIfFailed(cast err);
				},
				_ => None
			);

			Success(SVarDecl(_1, {span: _2, name: name}, type, expr), rest);
		},

		at([T_If(_1), ...rest]) => switch parseExpr(rest) {
			case Success(trueCond, rest2): switch parseBlock(rest2) {
				case Success(trueBlk, rest3):
					final conds = [];

					while(true) rest3._match(
						at([T_Orif(_2), ...rest4]) => switch parseExpr(rest4) {
							case Success(cond, rest5): switch parseBlock(rest5) {
								case Success(blk, rest6):
									rest3 = rest6;
									conds.push({span: _2, cond: cond, blk: blk});
								case err: return cast err;
							}
							case err: return cast err;
						},
						_ => break
					);

					final otherwise = rest3._match(
						at([T_Else(_2), ...rest4]) => switch parseBlock(rest4) {
							case Success(elseBlk, rest5):
								rest3 = rest5;
								Some(new Tuple2(_2, elseBlk));
							case err: return cast err;
						},
						_ => None
					);

					Success(SIf(_1, trueCond, trueBlk, conds, otherwise), rest3);

				case err: cast err;
			}
			case err: cast err;
		},
		
		at([T_Case(_1), T_LBrace(begin), ...rest]) => {
			final cases = [];

			while(true) rest._match(
				at([T_At(_2), ...rest2]) => switch parseCaseAtStmt(_2, rest2) {
					case Success(case_, rest3):
						cases.push(case_);
						
						rest3._match(
							at([T_RBrace(end), ...rest4]) => return Success(SCase(Span.range(_1, begin), cases, None, end), rest4),
							at([] | [isAnySep(_) => true]) => return Eof(tokens),
							at([isAnySep(_) => true, ...rest4]) => rest = rest4,
							_ => return Fatal(tokens, Some(rest3))
						);
					
					case err: return fatalIfFailed(cast err);
				},
				at([T_Else(_2), ...rest2]) => switch parseThenStmt(rest2) {
					case Success(then, Cons(T_RBrace(end), rest3)): return Success(SCase(Span.range(_1, begin), cases, Some(new Tuple2(_2, then)), end), rest3);
					case Success(_, rest3): return Fatal(tokens, Some(rest3));
					case err: return fatalIfFailed(cast err);
				},
				at([T_RBrace(end), ...rest2]) => return Success(SCase(Span.range(_1, begin), cases, None, end), rest2),
				_ => return Fatal(tokens, Some(rest))
			);

			Fatal(tokens, Some(rest));
		},

		at([T_Match(_1), ...rest]) => switch parseExpr(rest) {
			case Success(expr, Cons(T_LBrace(begin), rest2)):
				final cases = [];
	
				while(true) rest2._match(
					at([T_At(_2), ...rest3]) => switch parseMatchAtStmt(_2, rest3) {
						case Success(case_, rest4):
							cases.push(case_);
							
							rest4._match(
								at([T_RBrace(end), ...rest5]) => return Success(SMatch(_1, expr, begin, cases, None, end), rest5),
								at([] | [isAnySep(_) => true]) => return Eof(tokens),
								at([isAnySep(_) => true, ...rest5]) => rest2 = rest5,
								_ => return Fatal(tokens, Some(rest4))
							);
						
						case err: return fatalIfFailed(cast err);
					},
					at([T_Else(_2), ...rest3]) => switch parseThenStmt(rest3) {
						case Success(then, Cons(T_RBrace(end), rest4)): return Success(SMatch(_1, expr, begin, cases, Some(new Tuple2(_2, then)), end), rest4);
						case Success(_, rest4): return Fatal(tokens, Some(rest4));
						case err: return fatalIfFailed(cast err);
					},
					at([T_RBrace(end), ...rest3]) => return Success(SMatch(_1, expr, begin, cases, None, end), rest3),
					_ => return Fatal(tokens, Some(rest2))
				);
	
				Fatal(tokens, Some(rest2));
			
			case Success(expr, Cons(T_At(_2), rest2)): switch parseExpr(rest2) {
				case Success(pattern, rest3):
					final cond = rest3._match(
						at([T_If(_3), ...rest4]) => switch parseExpr(rest4) {
							case Success(made, rest5):
								rest3 = rest5;
								Some(new Tuple2(_3, made));
							case err: return fatalIfFailed(cast err);
						},
						_ => None
					);

					switch parseBlock(rest3) {
						case Success(thenBlk, Cons(T_Else(_4), rest4)): switch parseBlock(rest4) {
							case Success(elseBlk, rest5): Success(SShortMatch(_1, expr, _2, pattern, cond, thenBlk, Some(new Tuple2(_4, elseBlk))), rest5);
							case err: fatalIfFailed(cast err);
						}
						case Success(thenBlk, rest4): Success(SShortMatch(_1, expr, _2, pattern, cond, thenBlk, None), rest4);
						case err: fatalIfFailed(cast err);
					}

				case err: fatalIfFailed(cast err);
			}
			case Success(_, rest2): Fatal(tokens, Some(rest2));
			case err: fatalIfFailed(cast err);
		},

		at([T_While(_1), ...rest]) => switch parseExpr(rest) {
			case Success(cond, rest2):
				final label = rest2._match(
					at([T_Label(_2, "label"), T_Litsym(_3, name), ...rest3]) => {
						rest2 = rest3;
						Some(new Tuple2(_2, new Ident(_3, name)));
					},
					_ => None
				);
				switch parseBlock(rest2) {
					case Success(block, rest3): Success(SWhile(_1, cond, label, block), rest3);
					case err: fatalIfFailed(cast err);
				}
			case err: fatalIfFailed(cast err);
		},

		at([T_Do(_1), ...rest]) => {
			final label = rest._match(
				at([T_Label(_2, "label"), T_Litsym(_3, name), ...rest2]) => {
					rest = rest2;
					Some(new Tuple2(_2, new Ident(_3, name)));
				},
				_ => None
			);
			switch parseBlock(rest) {
				case Success(block, Cons(T_While(_2), rest2)): switch parseExpr(rest2) {
					case Success(cond, rest3): Success(SDoWhile(_1, label, block, _2, cond), rest3);
					case err: fatalIfFailed(cast err);
				}
				case Success(block, rest2): Success(SDo(_1, label, block), rest2);
				case err: fatalIfFailed(cast err);
			}
		},

		at([T_For(_1), ...rest]) => switch parseExpr(rest) {
			case Success(lvar, rest2): rest2._match(
				at([T_Label(startSpan, "from"), ...rest3]) => parseLoopRange(_1, lvar, startSpan, LoopFrom, rest3),
				at([T_Label(startSpan, "after"), ...rest3]) => parseLoopRange(_1, lvar, startSpan, LoopAfter, rest3),
				at([T_Comma(_), ...rest3]) => switch parseExpr(rest3) {
					case Success(lvar2, rest4): parseLoopIn(_1, lvar, Some(lvar2), rest4);
					case err: fatalIfFailed(cast err);
				},
				_ => parseLoopIn(_1, lvar, None, rest2)
			);
			case err: fatalIfFailed(cast err);
		},

		at([T_Return(_1), ...rest]) => switch parseFullExpr(rest) {
			case Success(expr, rest2): Success(SReturn(_1, Some(expr)), rest2);
			case Failure(_, _): Success(SReturn(_1, None), rest);
			case err: cast err;
		},
		
		at([T_Break(_1), T_Int(_2, depth, None), ...rest]) => Success(SBreak(_1, Some(new Tuple2(_2, Left(depth.parseInt())))), rest),
		at([T_Break(_1), T_Int(_2, depth, Some(exp)), ...rest]) => Success(SBreak(_1, Some(new Tuple2(_2, Left('${depth}e$exp'.parseInt())))), rest),
		at([T_Break(_1), T_Litsym(_2, label), ...rest]) => Success(SBreak(_1, Some(new Tuple2(_2, Right(label)))), rest),
		at([T_Break(_1), ...rest]) => Success(SBreak(_1, None), rest),

		at([T_Next(_1), T_Int(_2, depth, None), ...rest]) => Success(SNext(_1, Some(new Tuple2(_2, Left(depth.parseInt())))), rest),
		at([T_Next(_1), T_Int(_2, depth, Some(exp)), ...rest]) => Success(SNext(_1, Some(new Tuple2(_2, Left('${depth}e$exp'.parseInt())))), rest),
		at([T_Next(_1), T_Litsym(_2, label), ...rest]) => Success(SNext(_1, Some(new Tuple2(_2, Right(label)))), rest),
		at([T_Next(_1), ...rest]) => Success(SNext(_1, None), rest),

		at([T_Throw(_1), ...rest]) => switch parseFullExpr(rest) {
			case Success(expr, rest2): Success(SThrow(_1, expr), rest2);
			case err: fatalIfFailed(cast err);
		},

		at([T_Try(_1), ...rest]) => switch parseBlock(rest) {
			case Success(block, Cons(T_Catch(_), Cons(T_LBrace(begin), rest2))):
				final cases = [];
	
				while(true) rest2._match(
					at([T_At(_2), ...rest3]) => switch parseMatchAtStmt(_2, rest3) {
						case Success(case_, rest4):
							cases.push(case_);
							
							rest4._match(
								at([T_RBrace(end), ...rest5]) => return Success(STry(_1, block, begin, cases, None, end), rest5),
								at([] | [isAnySep(_) => true]) => return Eof(tokens),
								at([isAnySep(_) => true, ...rest5]) => rest2 = rest5,
								_ => return Fatal(tokens, Some(rest4))
							);
						
						case err: return fatalIfFailed(cast err);
					},
					at([T_Else(_2), ...rest3]) => switch parseThenStmt(rest3) {
						case Success(then, Cons(T_RBrace(end), rest4)): return Success(STry(_1, block, begin, cases, Some(new Tuple2(_2, then)), end), rest4);
						case Success(_, rest4): return Fatal(tokens, Some(rest4));
						case err: return fatalIfFailed(cast err);
					},
					at([T_RBrace(end), ...rest3]) => return Success(STry(_1, block, begin, cases, None, end), rest3),
					_ => return Fatal(tokens, Some(rest2))
				);
	
				Fatal(tokens, Some(rest2));

			case Success(_, Cons(T_Catch(_), rest2) | rest2): Fatal(tokens, Some(rest2));
			case err: fatalIfFailed(cast err);
		},

		at([]) => Eof(tokens),
		_ => switch parseFullExpr(tokens) {
			case Success(expr, rest): Success(SExpr(expr), rest);
			case err: fatalIfFailed(cast err);
		}
	);


	static function parseThenStmt(tokens: List<Token>) return tokens._match(
		at([T_EqGt(_1), ...rest]) => switch parseStmt(rest) {
			case Success(stmt, rest2): Success(ThenStmt(_1, stmt), rest2);
			case err: fatalIfFailed(cast err);
		},
		_ => switch parseBlock(tokens) {
			case Success(block, rest): Success(ThenBlock(block), rest);
			case err: fatalIfBad(tokens, cast err);
		}
	);

	static function parseCaseAtStmt(_1, tokens: List<Token>) return switch parseExpr(tokens) {
		case Success(cond, rest): switch parseThenStmt(rest) {
			case Success(then, rest2): Success({span: _1, cond: cond, then: then}, rest2);
			case err: fatalIfFailed(cast err);
		}
		case err: fatalIfFailed(cast err);
	}

	static function parseMatchAtStmt(_1, tokens: List<Token>) return switch parseExpr(tokens) {
		case Success(pattern, Cons(T_If(_2), rest) | Cons(T_LSep(_), Cons(T_If(_2), rest))): switch parseExpr(rest) {
			case Success(cond, rest2): switch parseThenStmt(rest2) {
				case Success(then, rest3): Success({span: _1, pattern: pattern, when: Some(new Tuple2(_2, cond)), then: then}, rest3);
				case err: fatalIfFailed(cast err);
			}
			case err: fatalIfFailed(cast err);
		}
		case Success(pattern, rest): switch parseThenStmt(rest) {
			case Success(then, rest2): Success({span: _1, pattern: pattern, when: None, then: then}, rest2);
			case err: fatalIfFailed(cast err);
		}
		case err: fatalIfFailed(cast err);
	}
	
	
	static function parseLoopIn(_1, lvar, lvar2, tokens: List<Token>) return tokens._match(
		at([T_Label(inSpan, "in"), ...rest]) => switch parseExpr(rest) {
			case Success(inExpr, rest2):
				final cond = rest2._match(
					at([T_Label(_2, "while"), ...rest3]) => switch parseExpr(rest3) {
						case Success(expr, rest4):
							rest2 = rest4;
							Some(new Tuple2(_2, expr));
						case err: return fatalIfFailed(cast err);
					},
					_ => None
				);
				
				final label = rest2._match(
					at([T_Label(_2, "label"), T_Litsym(_3, name), ...rest3]) => {
						rest2 = rest3;
						Some(new Tuple2(_2, new Ident(_3, name)));
					},
					_ => None
				);
				
				switch parseBlock(rest2) {
					case Success(block, rest3): Success(SForIn(_1, lvar, lvar2, inSpan, inExpr, cond, label, block), rest3);
					case err: fatalIfFailed(cast err);
				}
			case err: fatalIfFailed(cast err);
		},
		_ => Fatal(tokens, None)
	);

	static function parseLoopRange(_1, lvar, startSpan, startKind, tokens) return switch parseExpr(tokens) {
		case Success(startExpr, rest):
			final stopResult = rest._match(
				at([T_Label(_2, "to"), ...rest2]) => {
					rest = rest2;
					{span: _2, kind: LoopTo};
				},
				at([T_Label(_2, "upto"), ...rest2]) => {
					rest = rest2;
					{span: _2, kind: LoopUpto};
				},
				at([T_Label(_2, "downto"), ...rest2]) => {
					rest = rest2;
					{span: _2, kind: LoopDownto};
				},
				at([T_Label(_2, "times"), ...rest2]) => {
					rest = rest2;
					{span: _2, kind: LoopTimes};
				},
				_ => return Fatal(tokens, Some(rest))
			);
			
			switch parseExpr(rest) {
				case Success(stopExpr, rest2):
					final stopSpan = stopResult.span;
					final stopKind = stopResult.kind;
					final step = rest2._match(
						at([T_Label(_2, "by"), ...rest3]) => switch parseExpr(rest3) {
							case Success(expr, rest4):
								rest2 = rest4;
								Some(new Tuple2(_2, expr));
							case err: return fatalIfFailed(cast err);
						},
						_ => None
					);
					final cond = rest2._match(
						at([T_Label(_2, "while"), ...rest3]) => switch parseExpr(rest3) {
							case Success(expr, rest4):
								rest2 = rest4;
								Some(new Tuple2(_2, expr));
							case err: return fatalIfFailed(cast err);
						},
						_ => None
					);
					
					final label = rest2._match(
						at([T_Label(_2, "label"), T_Litsym(_3, name), ...rest3]) => {
							rest2 = rest3;
							Some(new Tuple2(_2, new Ident(_3, name)));
						},
						_ => None
					);
					
					switch parseBlock(rest2) {
						case Success(block, rest3): Success(SForRange(
							_1,
							lvar,
							startSpan,
							startKind,
							startExpr,
							stopSpan,
							stopKind,
							stopExpr,
							step,
							cond,
							label,
							block
						), rest3);
						case err: fatalIfFailed(cast err);
					}
				case err: fatalIfFailed(cast err);
			}
		case err: fatalIfFailed(cast err);
	}


	/* EXPRESSIONS */

	static function parseBasicExpr(tokens: List<Token>) {
		return tokens._match(
			at([_.asSoftName() => T_Name(_1, name), ...rest]) => Success(EName(_1, name), rest),
			at([T_Litsym(_1, sym), ...rest]) => Success(ELitsym(_1, sym), rest),
			at([T_Int(_1, int, exp), ...rest]) => Success(EInt(_1, int.parseInt(), exp.map(e -> e.parseInt())), rest),
			at([T_Hex(_1, hex), ...rest]) => Success(EInt(_1, hex.parseHex(), None), rest),
			at([T_Dec(_1, int, dec, exp), ...rest]) => Success(EDec(_1, int.parseInt(), dec, exp.map(e -> e.parseInt())), rest),
			at([T_Str(_1, segs), ...rest]) => switch parseStrSegs(segs) {
				case Success(parts, _): Success(EStr(_1, parts), rest);
				case err: cast err;
			},
			at([T_Char(_1, char), ...rest]) => Success(EChar(_1, char), rest),
			at([T_Bool(_1, bool), ...rest]) => Success(EBool(_1, bool), rest),
			at([T_This(_1), ...rest]) => Success(EThis(_1), rest),
			at([T_AnonArg(_1, depth, nth), ...rest]) => Success(EAnonArg(_1, depth, nth), rest),
			at([head = T_TypeName(_) | T_Wildcard(_), ...rest]) => switch parseType(tokens) {
				case Success(type, rest2): Success(EType(type), rest2);
				case f = Failure(_, _): switch head {
					case T_Wildcard(_1): Success(EWildcard(_1), rest);
					default: cast f;
				}
				case err: cast err;
			},
			at([T_HashLBracket(begin), T_RBracket(end), ...rest]) => Success(EArray(begin, [], end), rest),
			at([T_HashLBracket(begin), ...rest]) => switch parseArrayContents(rest) {
				case Success({exprs: exprs, end: end}, rest2): Success(EArray(begin, exprs, end), rest2);
				case err: cast err;
			},
			at([T_HashLParen(begin), T_RParen(end), ...rest]) => Success(EHash(begin, [], end), rest),
			at([T_HashLParen(begin), ...rest]) => switch parseHashContents(rest) {
				case Success({pairs: pairs, end: end}, rest2): Success(EHash(begin, pairs, end), rest2);
				case err: cast err;
			},
			at([T_HashLBrace(begin), T_RBrace(end), ...rest]) => Success(ETuple(begin, [], end), rest),
			at([T_HashLBrace(begin), ...rest]) => switch parseTupleContents(rest) {
				case Success({exprs: exprs, end: end}, rest2): Success(ETuple(begin, exprs, end), rest2);
				case err: cast err;
			},
			at([T_LParen(begin), ...rest]) => switch parseParenContents(rest) {
				case Success({exprs: exprs, end: end}, rest2): Success(EParen(begin, exprs, end), rest2);
				case err: cast err;
			},
			at([T_LBracket(begin), ...rest]) => switch parseFullExpr(rest) {
				case Success(EType(type), Cons(T_LSep(_), rest2) | rest2): switch finishTypeMsg(rest2) {
					case Success({msg: msg, end: end}, rest3): Success(ETypeMessage(type, begin, msg, end), rest3);
					case err: fatalIfFailed(cast err);
				}
				case Success(expr, Cons(T_LSep(_), rest2) | rest2): switch finishExprMsg(rest2) {
					case Success({msg: msg, end: end}, rest3): Success(EObjMessage(expr, begin, msg, end), rest3);
					case err: fatalIfFailed(cast err);
				}
				case err: fatalIfFailed(cast err);
			},
			at([T_LBrace(begin), T_BarBar(_), ...rest]) => finishFunc(begin, [], rest),
			at([T_LBrace(begin), T_Bar(_), T_Bar(_), ...rest]) => finishFunc(begin, [], rest),
			at([T_LBrace(begin), T_Bar(_), ...rest]) => {
				final params = [];

				while(true) {
					rest._match(
						at([_.asSoftName() => T_Name(_1, name), ...rest2]) => rest2._match(
							at([T_LParen(_), ..._]) => switch parseTypeAnno(rest2) {
								case Success(type, rest3):
									rest = rest3;
									params.push({name: new Ident(_1, name), type: Some(type)});
								case err: return fatalIfFailed(cast err);
							},
							_ => {
								rest = rest2;
								params.push({name: new Ident(_1, name), type: None});
							}
						),
						_ => return Fatal(tokens, Some(rest))
					);

					rest._match(
						at([T_Bar(_), ...rest2]) => {
							rest = rest2;
							break;
						},
						at([T_Comma(_), ...rest2]) => rest = rest2,
						_ => return Fatal(tokens, Some(rest))
					);
				}

				finishFunc(begin, params, rest);
			},
			at([T_LBrace(_), ..._]) => switch parseBlock(tokens) {
				case Success(block, rest): Success(EBlock(block), rest);
				case err: fatalIfFailed(cast err);
			},
			at([T_My(_1), ...rest]) => rest._match(
				at([_.asSoftName() => T_Name(_2, name), ...rest]) => {
					final type = switch parseTypeAnno(rest) {
						case Success(t, rest2):
							rest = rest2;
							Some(t);
						case Failure(_, _): None;
						case err: return cast err;
					};
					final expr = rest._match(
						at([T_Eq(_), ...rest2]) => switch parseFullExpr(rest2) {
							case Success(e, rest3):
								rest = rest3;
								Some(e);
							case err: return fatalIfFailed(cast err);
						},
						_ => None
					);
		
					Success(ECapture(_1, {span: _2, name: name}, type, expr), rest);
				},
				_ => Fatal(tokens, None)
			),
			_ => Failure(tokens, None)
		);
	}

	static function finishFunc(begin, params, tokens) return switch parseTypeAnno(tokens) {
		case Success(ret, Cons(T_RBrace(end), rest)): Success(EFunc(begin, params, Some(ret), [], end), rest);
		case Success(ret, Cons(T_LSep(_), rest) | rest): finishFuncBody(begin, params, Some(ret), rest);
		case Failure(_, _): 
			finishFuncBody(begin, params, None, tokens._match(
				at([T_LSep(_), ...rest]) => rest,
				_ => tokens
			));
		case err: cast err;
	}

	static function finishFuncBody(begin, params, ret, tokens) {
		final stmts = [];
		var rest = tokens;

		while(true) switch parseStmt(rest) {
			case Success(stmt, rest2):
				stmts.push(stmt);
				
				rest2._match(
					at([T_RBrace(end), ...rest3]) => return Success(EFunc(begin, params, ret, stmts, end), rest3),
					at([] | [isAnySep(_) => true]) => return Eof(tokens),
					at([isAnySep(_) => true, ...rest3]) => rest = rest3,
					_ => return Fatal(tokens, Some(rest2))
				);
			
			case err: return fatalIfBad(rest, cast err);
		}

		return Fatal(tokens, Some(rest));
	}

	static function parseArrayContents(tokens: List<Token>) {
		final exprs = [];
		var rest = tokens;
		
		while(true) switch parseFullExpr(rest) {
			case Success(e, rest2):
				exprs.push(e);
				
				rest2._match(
					at([T_RBracket(end), ...rest3]) => return Success({exprs: exprs, end: end}, rest3),
					at([] | [isAnySep(_) => true]) => return Eof(tokens),
					at([isAnySep(_) => true, ...rest3]) => rest = rest3,
					_ => return Fatal(tokens, Some(rest2))
				);

			case err: return cast err;
		}
	}

	static function parseTupleContents(tokens: List<Token>) {
		final exprs = [];
		var rest = tokens;
		
		while(true) switch parseFullExpr(rest) {
			case Success(e, rest2):
				exprs.push(e);
				
				rest2._match(
					at([T_RBrace(end), ...rest3]) => return Success({exprs: exprs, end: end}, rest3),
					at([] | [isAnySep(_) => true]) => return Eof(tokens),
					at([isAnySep(_) => true, ...rest3]) => rest = rest3,
					_ => return Fatal(tokens, Some(rest2))
				);

			case err: return cast err;
		}
	}

	static function parseHashContents(tokens: List<Token>) {
		final pairs = [];
		var rest = tokens;
		
		while(true) switch parseExpr(rest) {
			case Success(k, Cons(T_EqGt(_), rest2)): switch parseFullExpr(rest2) {
				case Success(v, rest3):
					pairs.push({k: k, v: v});

					rest3._match(
						at([T_RParen(end), ...rest4]) => return Success({pairs: pairs, end: end}, rest4),
						at([] | [isAnySep(_) => true]) => return Eof(tokens),
						at([isAnySep(_) => true, ...rest4]) => rest = rest4,
						_ => return Fatal(tokens, Some(rest3))
					);
				
				case err: return cast err;
			}
			case Success(_, rest2): return Fatal(tokens, Some(rest2));
			case err: return cast err;
		}
	}

	static function parseStrSegs(segs: Array<StrSegment>) {
		final parts: Array<StrPart> = [];
		var buf = new Buffer();

		for(seg in segs) switch seg {
			case SStr(str): buf.addString(str);
			case SChar(char): buf.addChar(char);
			case SCode(tokens): switch parseFullExpr(tokens) {
				case Success(expr, Nil):
					if(buf.length != 0) {
						parts.push(PStr(buf.toString()));
						buf = new Buffer();
					}

					parts.push(PCode(expr));
				
				case Success(_, rest): return Fatal(tokens, Some(rest));
				case err: return cast err;
			}
		}

		if(buf.length != 0) {
			parts.push(PStr(buf.toString()));
		}

		return Success(parts, Nil);
	}


	static function skipParen(tokens: List<Token>): List<Token> {
		while(true) tokens._match(
			at([T_LParen(_) | T_HashLParen(_), ...rest]) => tokens = skipParen(rest).tail(),
			at([T_LBracket(_) | T_HashLBracket(_), ...rest]) => tokens = skipBracket(rest).tail(),
			at([T_LBrace(_) | T_HashLBrace(_), ...rest]) => tokens = skipBrace(rest).tail(),
			at([T_RParen(_), ..._]) => return tokens,
			at([_, ...rest]) => tokens = rest,
			at([]) => throw "Error!"
		);
	}

	static function skipBracket(tokens: List<Token>): List<Token> {
		while(true) tokens._match(
			at([T_LParen(_) | T_HashLParen(_), ...rest]) => tokens = skipParen(rest).tail(),
			at([T_LBracket(_) | T_HashLBracket(_), ...rest]) => tokens = skipBracket(rest).tail(),
			at([T_LBrace(_) | T_HashLBrace(_), ...rest]) => tokens = skipBrace(rest).tail(),
			at([T_RBracket(_), ..._]) => return tokens,
			at([_, ...rest]) => tokens = rest,
			at([]) => throw "Error!"
		);
	}

	static function skipBrace(tokens: List<Token>): List<Token> {
		while(true) tokens._match(
			at([T_LParen(_) | T_HashLParen(_), ...rest]) => tokens = skipParen(rest).tail(),
			at([T_LBracket(_) | T_HashLBracket(_), ...rest]) => tokens = skipBracket(rest).tail(),
			at([T_LBrace(_) | T_HashLBrace(_), ...rest]) => tokens = skipBrace(rest).tail(),
			at([T_RBrace(_), ..._]) => return tokens,
			at([_, ...rest]) => tokens = rest,
			at([]) => throw "Error!"
		);
	}


	static function removeNewlines(tokens: List<Token>) {
		while(true) tokens._match(
			at([T_LParen(_) | T_HashLParen(_), ...rest]) => tokens = skipParen(rest),
			at([T_LBracket(_) | T_HashLBracket(_), ...rest]) => tokens = skipBracket(rest),
			at([T_LBrace(_) | T_HashLBrace(_), ...rest]) => tokens = skipBrace(rest),
			at([T_RParen(_), ..._]) => break,
			at([_, T_LSep(_), T_Cascade(_), ...rest]) => tokens = rest,
			at([_, T_LSep(_), ...rest]) => {
				tokens.setTail(rest);
				tokens = rest;
			},
			at([_, ...rest]) => tokens = rest,
			at([]) => return false
		);

		return true;
	}

	static function parseParenContents(tokens: List<Token>) {
		final exprs = [];
		var rest = tokens;

		if(!removeNewlines(rest)) {
			return Eof(tokens);
		}

		final leadingOp = rest._match(
			at([T_AndAnd(_), ...rest2]) => {
				rest = rest2;
				Some(Infix.And);
			},
			at([T_BarBar(_), ...rest2]) => {
				rest = rest2;
				Some(Infix.Or);
			},
			at([T_CaretCaret(_), ...rest2]) => {
				rest = rest2;
				Some(Infix.Xor);
			},
			at([T_BangBang(_), ...rest2]) => {
				rest = rest2;
				Some(Infix.Nor);
			},
			_ => None
		);
		
		while(true) switch parseFullExpr(rest) {
			case Success(e, rest2):
				exprs.push(e);
				
				rest2._match(
					at([T_RParen(end), ...rest3]) => {
						switch leadingOp {
							case None:
							case Some(op):
								switch exprs[0] {
									case EInfix(_, _, _ == op => true, _):
									default: return Fatal(tokens, None);
								}
						}

						return Success({exprs: exprs, end: end}, rest3);
					},
					at([] | [isAnyComma(_) => true]) => return Eof(tokens),
					at([isAnyComma(_) => true, ...rest3]) => rest = rest3,
					_ => return Fatal(tokens, Some(rest2))
				);

			case err: return cast err;
		}
	}


	/* Highest ---> Lowest
	 *  1:  a.b, a[b]
	 *  2:  ++a, --a, -a, ~a, a++, a--, a?
	 *  3:  !a
	 *  4:  a -> [b], a -> [b] = c, a -> b = c, a -> {...}   (Single line only)
	 *  5:  #a b
	 *  6:  a ** b
	 *  7:  a * b, a / b, a // b, a % b
	 *  8:  a + b, a - b
	 *  9:  a & b, a | b, a ^ b, a >> b, a << b
	 * 10:  a %% b
	 * 11:  a ?= b, a != b, a > b, a >= b, a < b, a <= b
	 * 12:  a && b, a || b, a ^^ b, a !! b
	 * 13:  a = b, a += b, a -= b, a *= b, a **= b, a /= b, a //= b, a %= b, a %%= b, a &= b, a |= b, a ^= b, a >>= b, a <<= b, a &&= b, a ||= b, a ^^= b, a !!= b
	 * 14:  ...a
	*/

	static inline function parseExpr(tokens) {
		return parseExpr14(tokens);
	}


	static function parseMultiMsgLabels(tokens: List<Token>) {
		var rest = tokens;
		final first = tokens._match(
			at([T_Label(_1, label), ...rest2]) => switch parseFullExpr(rest2) {
				case Success(expr, rest3):
					rest = rest3;
					Label.Named(_1, label, expr);
				case err: return cast err;
			},
			at([T_Punned(_1, punned), ...rest2]) => {
				rest = rest2;
				Label.Punned(_1, punned);
			},
			_ => return Fatal(tokens, None)
		);
		final labels = [first];

		while(true) {
			rest._match(
				at([T_RBracket(end), ...rest2]) => return Success({labels: labels, end: end}, rest2),
				at([T_Label(_1, label), ...rest2] | [isAnySep(_) => true, T_Label(_1, label), ...rest2]) => switch parseFullExpr(rest2) {
					case Success(expr, rest3):
						rest = rest3;
						labels.push(Named(_1, label, expr));
					case err: return cast err;
				},
				at([T_Punned(_1, punned), ...rest2] | [isAnySep(_) => true, T_Punned(_1, punned), ...rest2]) => {
					rest = rest2;
					labels.push(Punned(_1, punned));
				},
				at([isAnyComma(_) => true, ...rest2]) => switch parseFullExpr(rest2) {
					case Success(expr, rest3):
						rest = rest3;
						labels.push(Anon(expr));
					case err: return cast err;
				},
				_ => return Fatal(tokens, Some(rest))
			);
		}
	}

	static function finishExprMsg(tokens: List<Token>): ParseResult<{msg: Message<Expr>, end: Span}> return tokens._match(
		at([T_TypeName(_, _) | T_Wildcard(_), ..._]) => switch parseType(tokens) {
			case Success(type, Cons(T_LSep(_), rest) | rest): rest._match(
				at([T_Label(_, _) | T_Punned(_, _), ..._]) => switch parseMultiMsgLabels(rest) {
					case Success({labels: labels, end: end}, rest2): Success({msg: Multi(Some(type), labels), end: end}, rest2);
					case err: cast err;
				},
				at([_.asAnyName() => T_Name(_1, name), ...rest3]) => rest3._match(
					at([T_RBracket(end), ...rest4]) => Success({msg: Single(Some(type), _1, name), end: end}, rest4),
					at(_) => Fatal(tokens, Some(rest3))
				),
				at([T_TypeName(_, _), ..._]) => switch parseType(rest) {
					case Success(type2, Cons(T_RBracket(end), rest2)): Success({msg: Cast(Some(type), type2), end: end}, rest2);
					case Success(_, rest2): Fatal(tokens, Some(rest2));
					case err: cast err;
				},
				at([T_RBracket(end), ...rest2]) => Success({msg: Cast(None, type), end: end}, rest2),
				_ => Fatal(tokens, None)
			);
			case err: cast err;
		},
		at([T_Label(_, _) | T_Punned(_, _), ..._]) => switch parseMultiMsgLabels(tokens) {
			case Success({labels: labels, end: end}, rest): Success({msg: Multi(None, labels), end: end}, rest);
			case err: cast err;
		},
		at([_.asAnyName() => T_Name(_1, name), ...rest3]) => rest3._match(
			at([T_RBracket(end), ...rest4]) => Success({msg: Single(None, _1, name), end: end}, rest4),
			at(_) => Fatal(tokens, Some(rest3))
		),
		_ => Fatal(tokens, None)
	);

	static function finishTypeMsg(tokens): ParseResult<{msg: Message<Type>, end: Span}> return switch finishExprMsg(tokens) {
		case Success({msg: Cast(_, _)}, _): Fatal(tokens, None);
		case res: cast res;
	}

	static function parseExpr1(tokens) {
		final result = switch parseBasicExpr(tokens) {
			case Success(e = EType(type), rest): rest._match(
				at([T_Dot(_), T_Name(_1, name), ...rest2]) => {
					{expr: ETypeMember(type, {span: _1, name: name}), rest: rest2};
				},
				at([T_LBracket(begin), ...rest2]) => switch finishTypeMsg(rest2) {
					case Success({msg: msg, end: end}, rest3):
						{expr: ETypeMessage(type, begin, msg, end), rest: rest3};
					case err: return fatalIfBad(tokens, cast err);
				},
				at([
					  T_Int(_, _, _)
					| T_Hex(_, _)
					| T_Dec(_, _, _, _)
					| T_Str(_, _)
					| T_Char(_, _)
					| T_Bool(_, _)
					| T_HashLBracket(_)
					| T_HashLParen(_)
					| T_HashLBrace(_),
					..._
				] | [T_LBrace(_), T_Bar(_) | T_BarBar(_), ..._]) => switch parseBasicExpr(rest) {
					case Success(expr, rest2): {expr: ELiteralCtor(type, expr), rest: rest2};
					case err: return fatalIfBad(tokens, cast err);
				},
				_ => {expr: e, rest: rest}
			);
			case Success(e, rest): {expr: e, rest: rest};
			case err: return err;
		};
		var expr = result.expr;
		var rest = result.rest;

		while(true) rest._match(
			at([T_Dot(_), T_Name(_1, name), ...rest2]) => {
				rest = rest2;
				expr = EObjMember(expr, {span: _1, name: name});
			},
			at([T_LBracket(begin), ...rest2]) => switch finishExprMsg(rest2) {
				case Success({msg: msg, end: end}, rest3):
					rest = rest3;
					expr = EObjMessage(expr, begin, msg, end);
				case err: return fatalIfBad(tokens, cast err);
			},
			_ => break
		);

		return Success(expr, rest);
	}


	static inline function parseExpr2Prefix(_1, op: Prefix, rest) return switch parseExpr2Head(rest) {
		case Success(expr, rest2): Success(EPrefix(_1, op, expr), rest2);
		case err: cast err;
	}

	static function parseExpr2Head(tokens: List<Token>) return tokens._match(
		at([T_Minus(_1), ...rest]) => parseExpr2Prefix(_1, PNeg, rest),
		at([T_PlusPlus(_1), ...rest]) => parseExpr2Prefix(_1, PIncr, rest),
		at([T_MinusMinus(_1), ...rest]) => parseExpr2Prefix(_1, PDecr, rest),
		at([T_Tilde(_1), ...rest]) => parseExpr2Prefix(_1, PCompl, rest),
		_ => parseExpr1(tokens)
	);
	
	// TODO: support expr1 after postfix op
	static function parseExpr2(tokens) return switch parseExpr2Head(tokens) {
		case Success(expr, rest):
			while(true) rest._match(
				at([T_Question(_1), ...rest2]) => {
					expr = ESuffix(expr, _1, STruthy);
					rest = rest2;
				},
				at([T_PlusPlus(_1), ...rest2]) => {
					expr = ESuffix(expr, _1, SIncr);
					rest = rest2;
				},
				at([T_MinusMinus(_1), ...rest2]) => {
					expr = ESuffix(expr, _1, SDecr);
					rest = rest2;
				},
				_ => break
			);

			Success(expr, rest);

		case err: err;
	};


	static function parseExpr3(tokens: List<Token>) return tokens._match(
		at([T_Bang(_1), ...rest]) => switch parseExpr2(rest) {
			case Success(expr, rest2): Success(EPrefix(_1, PNot, expr), rest2);
			case err: cast err;
		},
		_ => parseExpr2(tokens)
	);


	static function parseExpr4CascadeContents(_1, level, tokens: List<Token>): ParseResult<Cascade<Expr>> {
		var rest = tokens;
		
		final kind: CascadeKind<Expr> = tokens._match(
			at([T_LBracket(_), ...rest2]) => switch finishExprMsg(rest2) {
				case Success({msg: msg}, rest3): rest3._match(
					at([T_PlusPlus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Incr);
					},
					at([T_MinusMinus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Decr);
					},
					at([getAssignableOp(_) => Some({span: _2, op: op}), ...rest4]) => switch parseExpr3(rest4) {
						case Success(expr, rest5):
							rest = rest5;
							AssignMessage(msg, _2, op, expr);
						case err: return cast err;
					},
					_ => {
						rest = rest3;
						Message(msg);
					}
				);
				case err: return cast err;
			},
			at([T_LBrace(_), ..._]) => switch parseBlock(tokens) {
				case Success(block, rest2):
					rest = rest2;
					Block(block);
				case err: return cast err;
			},
			at([_.asSoftName() => T_Name(_2, name), T_PlusPlus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Incr);
			},
			at([_.asSoftName() => T_Name(_2, name), T_MinusMinus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Decr);
			},
			at([_.asSoftName() => T_Name(_2, name), getAssignableOp(_) => Some({span: _3, op: op}), ...rest2]) => switch parseExpr3(rest2) {
				case Success(expr, rest3):
					rest = rest3;
					AssignMember({span: _2, name: name}, _3, op, expr);
				case err: return cast err;
			},
			_ => return Fatal(tokens, None)
		);
		final nested = [];
		final nextLevel = level + 1;

		while(true) rest._match(
			at([T_Cascade(_2, _ == nextLevel => true), ...rest2]) => switch parseExpr4CascadeContents(_2, level + 1, rest2) {
				case Success(cascade, rest3):
					rest = rest3;
					nested.push(cascade);
				case err: return fatalIfFailed(err);
			},
			_ => break
		);

		return Success({
			span: _1,
			depth: level,
			kind: kind,
			nested: nested
		}, rest);
	}

	static function parseExpr4TypeCascadeContents(_1, level, tokens: List<Token>): ParseResult<Cascade<Type>> {
		var rest = tokens;
		
		final kind: CascadeKind<Type> = tokens._match(
			at([T_LBracket(_), ...rest2]) => switch finishTypeMsg(rest2) {
				case Success({msg: msg}, rest3): rest3._match(
					at([T_PlusPlus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Incr);
					},
					at([T_MinusMinus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Decr);
					},
					at([getAssignableOp(_) => Some({span: _2, op: op}), ...rest4]) => switch parseExpr3(rest4) {
						case Success(expr, rest5):
							rest = rest5;
							AssignMessage(msg, _2, op, expr);
						case err: return cast err;
					},
					_ => {
						rest = rest3;
						Message(msg);
					}
				);
				case err: return cast err;
			},
			at([T_LBrace(_), ..._]) => switch parseBlock(tokens) {
				case Success(block, rest2):
					rest = rest2;
					Block(block);
				case err: return cast err;
			},
			at([_.asSoftName() => T_Name(_2, name), T_PlusPlus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Incr);
			},
			at([_.asSoftName() => T_Name(_2, name), T_MinusMinus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Decr);
			},
			at([_.asSoftName() => T_Name(_2, name), getAssignableOp(_) => Some({span: _3, op: op}), ...rest2]) => switch parseExpr3(rest2) {
				case Success(expr, rest3):
					rest = rest3;
					AssignMember({span: _2, name: name}, _3, op, expr);
				case err: return cast err;
			},
			_ => return Fatal(tokens, None)
		);
		final nested = [];
		final nextLevel = level + 1;

		while(true) rest._match(
			at([T_Cascade(_2, _ == nextLevel => true), ...rest2]) => switch parseExpr4CascadeContents(_2, level + 1, rest2) {
				case Success(cascade, rest3):
					rest = rest3;
					nested.push(cascade);
				case err: return fatalIfFailed(cast err);
			},
			_ => break
		);

		return Success({
			span: _1,
			depth: level,
			kind: kind,
			nested: nested
		}, rest);
	}

	static function parseExpr4(tokens) return switch parseExpr3(tokens) {
		case Success(EType(type), Cons(T_Cascade(_1, 1), rest)):
			final cascades = [switch parseExpr4TypeCascadeContents(_1, 1, rest) {
				case Success(cascade, rest2):
					rest = rest2;
					cascade;
				case err: return cast err;
			}];

			while(true) rest._match(
				at([T_Cascade(_2, 1), ...rest2]) => switch parseExpr4TypeCascadeContents(_2, 1, rest2) {
					case Success(cascade, rest3):
						rest = rest3;
						cascades.push(cascade);
					case err: return cast err;
				},
				_ => break
			);

			Success(ETypeCascade(type, cascades), rest);

		case Success(expr, Cons(T_Cascade(_1, 1), rest)):
			final cascades = [switch parseExpr4CascadeContents(_1, 1, rest) {
				case Success(cascade, rest2):
					rest = rest2;
					cascade;
				case err: return cast err;
			}];

			while(true) rest._match(
				at([T_Cascade(_2, 1), ...rest2]) => switch parseExpr4CascadeContents(_2, 1, rest2) {
					case Success(cascade, rest3):
						rest = rest3;
						cascades.push(cascade);
					case err: return cast err;
				},
				_ => break
			);

			Success(EObjCascade(expr, cascades), rest);

		case res: res;
	}


	static function parseExpr5(tokens: List<Token>) return tokens._match(
		at([T_Tag(_1, tag), ...rest]) => switch parseExpr5(rest) {
			case Success(expr, rest2): Success(ETag(_1, tag, expr), rest2);
			case err: fatalIfBad(rest, err);
		},
		_ => parseExpr4(tokens)
	);


	static function parseExpr6(tokens) return switch parseExpr5(tokens) {
		case Success(left, Cons(T_StarStar(_1), rest)): switch parseExpr6(rest) {
			case Success(right, rest2): Success(EInfix(left, _1, Pow, right), rest2);
			case err: fatalIfBad(rest, err);
		}
		case res: res;
	}


	static function parseExpr7(tokens) return switch parseExpr6(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_Star(_1), ...rest2]) => switch parseExpr6(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Times, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Div(_1), ...rest2]) => switch parseExpr6(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Div, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_DivDiv(_1), ...rest2]) => switch parseExpr6(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, IntDiv, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Mod(_1), ...rest2]) => switch parseExpr6(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Mod, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static function parseExpr8(tokens) return switch parseExpr7(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_Plus(_1), ...rest2]) => switch parseExpr7(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Plus, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Minus(_1), ...rest2]) => switch parseExpr7(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Minus, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static function parseExpr9(tokens) return switch parseExpr8(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_And(_1), ...rest2]) => switch parseExpr8(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, BitAnd, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Bar(_1), ...rest2]) => switch parseExpr8(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, BitOr, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Caret(_1), ...rest2]) => switch parseExpr8(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, BitXor, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_LtLt(_1), ...rest2]) => switch parseExpr8(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Shl, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_GtGt(_1), ...rest2]) => switch parseExpr8(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Shr, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static function parseExpr10(tokens) return switch parseExpr9(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_ModMod(_1), ...rest2]) => switch parseExpr9(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, IsMod, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static function parseExpr11(tokens) return switch parseExpr10(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_QuestionEq(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Eq, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_BangEq(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Ne, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Gt(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Gt, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_GtEq(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Ge, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_Lt(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Lt, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_LtEq(_1), ...rest2]) => switch parseExpr10(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Le, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static function parseExpr12(tokens) return switch parseExpr11(tokens) {
		case Success(left, rest):
			while(true) rest._match(
				at([T_AndAnd(_1), ...rest2]) => switch parseExpr11(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, And, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_BarBar(_1), ...rest2]) => switch parseExpr11(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Or, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_CaretCaret(_1), ...rest2]) => switch parseExpr11(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Xor, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				at([T_BangBang(_1), ...rest2]) => switch parseExpr11(rest2) {
					case Success(right, rest3):
						left = EInfix(left, _1, Nor, right);
						rest = rest3;
					case err: return fatalIfBad(rest, err);
				},
				_ => break
			);

			Success(left, rest);

		case err: err;
	}


	static inline function getAssignableOp(token): Option<{span: Span, op: Option<Assignable>}> return switch token {
		case T_Eq(span): Some({span: span, op: None});
		case T_PlusEq(span): Some({span: span, op: Some(Plus)});
		case T_MinusEq(span): Some({span: span, op: Some(Minus)});
		case T_StarEq(span): Some({span: span, op: Some(Times)});
		case T_StarStarEq(span): Some({span: span, op: Some(Pow)});
		case T_DivEq(span): Some({span: span, op: Some(Div)});
		case T_DivDivEq(span): Some({span: span, op: Some(IntDiv)});
		case T_ModEq(span): Some({span: span, op: Some(Mod)});
		case T_ModModEq(span): Some({span: span, op: Some(IsMod)});
		case T_AndEq(span): Some({span: span, op: Some(BitAnd)});
		case T_AndAndEq(span): Some({span: span, op: Some(And)});
		case T_BarEq(span): Some({span: span, op: Some(BitOr)});
		case T_BarBarEq(span): Some({span: span, op: Some(Or)});
		case T_CaretEq(span): Some({span: span, op: Some(BitXor)});
		case T_CaretCaretEq(span): Some({span: span, op: Some(Xor)});
		case T_BangBangEq(span): Some({span: span, op: Some(Nor)});
		case T_LtLtEq(span): Some({span: span, op: Some(Shl)});
		case T_GtGtEq(span): Some({span: span, op: Some(Shr)});
		default: None;
	};

	static function parseExpr13(tokens) return switch parseExpr12(tokens) {
		case Success(left, Cons(getAssignableOp(_) => Some({span: span, op: op}), rest)): switch parseExpr13(rest) {
			case Success(right, rest2): Success(EInfix(left, span, Assign(op), right), rest2);
			case err: fatalIfBad(rest, err);
		}
		case res: res;
	}


	static function parseExpr14(tokens: List<Token>) return tokens._match(
		at([T_DotDotDot(_1), ...rest]) => switch parseExpr13(rest) {
			case Success(expr, rest2): Success(EPrefix(_1, PSpread, expr), rest2);
			case err: cast err;
		},
		_ => parseExpr13(tokens)
	);


	static function parseFullExpr(tokens: List<Token>) return switch parseExpr(tokens) {
		case Success(expr = EType(type), rest): rest._match(
			at([T_LSep(_), T_Cascade(_1, 1), ...rest2]) => {
				final cascades = [switch parseTypeCascadeContents(_1, 1, rest2) {
					case Success(cascade, rest3):
						rest2 = rest3;
						cascade;
					case err: return fatalIfBad(rest2, cast err);
				}];

				while(true) rest2._match(
					at([T_LSep(_), T_Cascade(_2, 1), ...rest3]) => switch parseTypeCascadeContents(_2, 1, rest3) {
						case Success(cascade, rest4):
							rest2 = rest4;
							cascades.push(cascade);
						case err: return fatalIfBad(rest3, cast err);
					},
					_ => break
				);

				Success(ETypeCascade(type, cascades), rest2);
			},
			at([T_LSep(_), T_Cascade(_, _), ..._]) => Fatal(tokens, Some(rest)),
			_ => Success(expr, rest)
		);
		case Success(expr, rest): rest._match(
			at([T_LSep(_), T_Cascade(_1, 1), ...rest2]) => {
				final cascades = [switch parseExprCascadeContents(_1, 1, rest2) {
					case Success(cascade, rest3):
						rest2 = rest3;
						cascade;
					case err: return fatalIfBad(rest2, cast err);
				}];

				while(true) rest2._match(
					at([T_LSep(_), T_Cascade(_2, 1), ...rest3]) => switch parseExprCascadeContents(_2, 1, rest3) {
						case Success(cascade, rest4):
							rest2 = rest4;
							cascades.push(cascade);
						case err: return fatalIfBad(rest3, cast err);
					},
					_ => break
				);

				Success(EObjCascade(expr, cascades), rest2);
			},
			at([T_LSep(_), T_Cascade(_, _), ..._]) => Fatal(tokens, Some(rest)),
			_ => Success(expr, rest)
		);
		case err: err;
	}

	static function parseExprCascadeContents(_1, level, tokens: List<Token>): ParseResult<Cascade<Expr>> {
		var rest = tokens;
		
		final kind: CascadeKind<Expr> = tokens._match(
			at([T_LBracket(_), ...rest2]) => switch finishExprMsg(rest2) {
				case Success({msg: msg}, rest3): rest3._match(
					at([T_PlusPlus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Incr);
					},
					at([T_MinusMinus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Decr);
					},
					at([getAssignableOp(_) => Some({span: _2, op: op}), ...rest4]) => switch parseExpr(rest4) {
						case Success(expr, rest5):
							rest = rest5;
							AssignMessage(msg, _2, op, expr);
						case err: return cast err;
					},
					_ => {
						rest = rest3;
						Message(msg);
					}
				);
				case err: return cast err;
			},
			at([T_LBrace(_), ..._]) => switch parseBlock(tokens) {
				case Success(block, rest2):
					rest = rest2;
					Block(block);
				case err: return cast err;
			},
			at([_.asSoftName() => T_Name(_2, name), T_PlusPlus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Incr);
			},
			at([_.asSoftName() => T_Name(_2, name), T_MinusMinus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Decr);
			},
			at([_.asSoftName() => T_Name(_2, name), getAssignableOp(_) => Some({span: _3, op: op}), ...rest2]) => switch parseExpr(rest2) {
				case Success(expr, rest3):
					rest = rest3;
					AssignMember({span: _2, name: name}, _3, op, expr);
				case err: return cast err;
			},
			_ => return Fatal(tokens, None)
		);
		final nested = [];
		final nextLevel = level + 1;

		while(true) rest._match(
			at([T_LSep(_), T_Cascade(_2, _ == nextLevel => true), ...rest2]) => switch parseExprCascadeContents(_2, level + 1, rest2) {
				case Success(cascade, rest3):
					rest = rest3;
					nested.push(cascade);
				case err: return err;
			},
			_ => break
		);

		return Success({
			span: _1,
			depth: level,
			kind: kind,
			nested: nested
		}, rest);
	}

	static function parseTypeCascadeContents(_1, level, tokens: List<Token>): ParseResult<Cascade<Type>> {
		var rest = tokens;
		
		final kind: CascadeKind<Type> = tokens._match(
			at([T_LBracket(_), ...rest2]) => switch finishTypeMsg(rest2) {
				case Success({msg: msg}, rest3): rest3._match(
					at([T_PlusPlus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Incr);
					},
					at([T_MinusMinus(_2), ...rest4]) => {
						rest = rest4;
						StepMessage(msg, _2, Decr);
					},
					at([getAssignableOp(_) => Some({span: _2, op: op}), ...rest4]) => switch parseExpr(rest4) {
						case Success(expr, rest5):
							rest = rest5;
							AssignMessage(msg, _2, op, expr);
						case err: return cast err;
					},
					_ => {
						rest = rest3;
						Message(msg);
					}
				);
				case err: return cast err;
			},
			at([T_LBrace(_), ..._]) => switch parseBlock(tokens) {
				case Success(block, rest2):
					rest = rest2;
					Block(block);
				case err: return cast err;
			},
			at([_.asSoftName() => T_Name(_2, name), T_PlusPlus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Incr);
			},
			at([_.asSoftName() => T_Name(_2, name), T_MinusMinus(_3), ...rest2]) => {
				rest = rest2;
				StepMember({span: _2, name: name}, _3, Decr);
			},
			at([_.asSoftName() => T_Name(_2, name), getAssignableOp(_) => Some({span: _3, op: op}), ...rest2]) => switch parseExpr(rest2) {
				case Success(expr, rest3):
					rest = rest3;
					AssignMember({span: _2, name: name}, _3, op, expr);
				case err: return cast err;
			},
			_ => return Fatal(tokens, None)
		);
		final nested = [];
		final nextLevel = level + 1;

		while(true) rest._match(
			at([T_LSep(_), T_Cascade(_2, _ == nextLevel => true), ...rest2]) => switch parseExprCascadeContents(_2, level + 1, rest2) {
				case Success(cascade, rest3):
					rest = rest3;
					nested.push(cascade);
				case err: return fatalIfFailed(cast err);
			},
			_ => break
		);

		return Success({
			span: _1,
			depth: level,
			kind: kind,
			nested: nested
		}, rest);
	}


	/* ERRORS */

	static function updateIfBad<T>(tokens, result: ParseResult<T>) return switch result {
		case Failure(begin, None): Failure(tokens, Some(begin));
		case Failure(_, rest): Failure(tokens, rest);
		case Fatal(begin, None): Fatal(tokens, Some(begin));
		default: result;
	}

	static function fatalIfBad<T>(tokens, result: ParseResult<T>) return switch result {
		case Failure(begin, None) | Fatal(begin, None): Fatal(tokens, Some(begin));
		case Failure(_, rest): Fatal(tokens, rest);
		default: result;
	}

	static function fatalIfFailed<T>(result: ParseResult<T>) return switch result {
		case Failure(begin, end): Fatal(begin, end);
		default: result;
	}


	/* MISC */
	
	static function trimTokens(tokens: List<Token>) {
		final ogTokens = tokens;

		while(true) tokens = tokens._match(
			at([( T_LParen(_)
				| T_LBracket(_)
				| T_LBrace(_)
				| T_HashLParen(_)
				| T_HashLBracket(_)
				| T_HashLBrace(_)
			), T_LSep(_), ...rest]) => {
				tokens.setTail(rest);
				rest;
			},
			at([T_LSep(_), t = T_RParen(_) | T_RBracket(_) | T_RBrace(_), T_LSep(_)]) => {
				tokens.setHead(t);
				tokens.setTail(Nil);
				return ogTokens;
			},
			at([T_LSep(_), t = T_RParen(_) | T_RBracket(_) | T_RBrace(_), ...rest]) => {
				tokens.setHead(t);
				tokens.setTail(rest);
				rest;
			},
			at([T_Str(_, segs), ...rest]) => {
				for(i in 0...segs.length) switch segs[i] {
					case SCode(Cons(T_LSep(_), inner) | inner): segs[i] = SCode(trimTokens(inner));
					default:
				}
				rest;
			},
			at([_, T_LSep(_)]) => {
				tokens.setTail(Nil);
				return ogTokens;
			},
			at([T_LSep(_)]) => throw "impossible!",
			at([_, ...rest]) => rest,
			at([]) => return ogTokens
		);
	}

	
	static inline function isAnySep(token: Token) return token.match(T_LSep(_) | T_CSep(_) | T_Comma(_));

	static inline function isAnyComma(token: Token) return token.match(T_CSep(_) | T_Comma(_));
}