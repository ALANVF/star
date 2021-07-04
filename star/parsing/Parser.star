use Lexer
use Info from: Diagnostic
use Priority from: Info
use Decl

module Parser {
	on [parse: tokens (Tokens)] (Program) {
		match tokens {
			at #[Token[lSep], ...my rest] => return This[parse: rest]
			at #[Token[use], Token[litsym: "script"], ...my rest] => return This[parseScript: rest]
			else => return This[parseModular: tokens]
		}
	}
	
	on [parseScript: tokens (Tokens)] (Program) {
		throw "NYI!"
	}
	
	on [parseModular: tokens (Tokens)] (Program) {
		my expectSep = false
		my lastWasError = false
		my errors = #[]
		my decls = #[]
		my badTokens = IdentitySet #[]
		
		while tokens? {
			my oldTokens = tokens
			
			if expectSep {
				my first = tokens[first]
				
				tokens++
				
				if !first[isAnySep] {
					errors[add: Diagnostic[
						severity: Severity.error
						message: "Syntax error"
						info: #[
							Info[
								span: first.span
								message: "Unexpected \(first.basicName), was expecting a comma or newline instead"
								priority: Priority.primary
							]
						]
					]]
					
					badTokens[add: first]
				}
			}
			
			match This[nextDecl: #[], tokens] {
				at Result[success: my decl, my rest] {
					decls[add: decl]
					tokens = rest
					expectSep ||= true
					lastWasError &&= false
				}
				
				at (
					|| Result[failure: my begin, Maybe[none]]
					|| Result[fatal: my begin, Maybe[none]]
				) {
					my first = begin[first]
					
					if badTokens[contains: first] !! (lastWasError && first[isAnySep]) {
						errors[add: Diagnostic[
							severity: Severity.error
							message: "Syntax error"
							info: #[
								Info[
									span: first.span
									message: "Unexpected \(first.basicName)"
									priority: Priority.primary
								]
							]
						]]
						
						badTokens[add: first]
					}
					
					tokens++
					
					while true {
						match tokens at #[] || #[Token[lSep], ..._] {
							break
						} else {
							tokens++
						}
					}
					
					expectSep = false
					lastWasError = true
				}
				
				at (
					|| Result[failure: my begin, Maybe[the: my end]]
					|| Result[fatal: my begin, Maybe[the: my end]]
				) {
					my first = begin[first]
					my last = [end? yes: end[first] no: begin[last]]
					
					if !badTokens[contains: last] {
						errors[add: Diagnostic[
							severity: Severity.error
							message: "Syntax error"
							info: #[
								Info[
									span: last.span
									message: "Unexpected \(last.basicName)"
									priority: Priority.primary
								]
							] -> {
								;-- Don't add another message if they're the same token
								if first != last {
									this[add: Info[
										span: first.span
										message: "Starting here"
										priority: Priority.secondary
									]]
								}
							}
						]]
						
						badTokens[add: last]
					}
					
					while true {
						match tokens at #[] || #[Token[lSep], ..._] {
							break
						} else {
							tokens++
						}
					}
					
					expectSep = false
					lastWasError = true
				}
				
				at Result[eof: my begin] {
					my realBegin = {
						case {
							at begin? => return begin
							at tokens? => return tokens
							else => return oldTokens
						}
					}
					my first = realBegin[first]
					my last = realBegin[last]
					
					if !badTokens[contains: first] {
						errors[add: Diagnostic[
							severity: Severity.error
							message: "Syntax error"
							info: #[
								Info[
									span: last.span
									message: "Unexpected end of file after \(last.basicName)"
									priority: Priority.primary
								]
							] -> {
								;-- Don't add another message if they're the same token
								if first != last {
									this[add: Info[
										span: first.span
										message: "Starting here"
										priority: Priority.secondary
									]]
								}
							}
						]]
						
						badTokens[add: first]
					}
					
					tokens = tokens[tail]
				}
				
				at Result[fatalError: my error] {
					errors[add: error]
					
					while true {
						match tokens at #[] || #[Token[lSep], ..._] {
							break
						} else {
							tokens++
						}
					}
					
					expectSep = false
					lastWasError = true
				}
			}
		}
		
		return Program[modular: decls :errors]
	}
	
	
	;== Decls
	
	on [nextDeclBody: tokens (Tokens)] (Result[Body]) {
		match tokens {
			at #[my l = Token[lBrace], my r = Token[rBrace], ...my rest] {
				return Result[success: Body[begin: l.span of: #[] end: r.span], rest]
			}
			
			at #[my l = Token[lBrace], ...my rest] {
				my decls = #[]
				
				while true {
					match This[nextDecl: #[], rest] {
						at Result[success: my decl, my rest'] {
							decls[add: decl]
							
							match rest' {
								at #[my r = Token[rBrace], ...my rest''] => return Result[success: Body[begin: l.span of: decls end: r.span], rest'']
								at #[] || #[_[isAnySep]] => return Result[eof: tokens]
								at #[_[isAnySep], ...my rest''] => rest = rest''
								else => return Result[fatal: tokens, Maybe[the: rest']]
							}
						}
						at my fail => return fail[Result[Body]]
					}
				}
			}
			
			at #[] => return Result[eof: tokens]
			else => return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [nextDecl: generics (Array[Generic.Param]), tokens (Tokens)] (Result[Decl]) {
		match tokens {
			at #[my t = Token[type], ...my rest] => match This[parseGenericParam: t.span, rest] {
				at Result[success: my param, #[_[isAnySep], ...my rest']] => return This[nextDecl: generics->[add: decl], rest']
				at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at my fail => return fail[Result[Decl]]
			}
			at #[my t = Token[use], Token[litsym: my sym span: my span], _[isAnySep], ...my rest] => return This[parseUsePragma: generics, t.span, span, sym, rest]
			at #[my t = Token[use], ...my rest] => return This[parseUseDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[alias], ...my rest] => return This[parseAliasDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[module], ...my rest] => return This[parseModuleDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[class], ...my rest] => return This[parseClassDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[protocol], ...my rest] => return This[parseProtocolDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[category], ...my rest] => return This[parseCategoryDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[kind], ...my rest] => return This[parseKindDecl: generics, t.span, rest][fatalIfBad: tokens]
			at #[my t = Token[my], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid member"
						info: #[
							Info[
								span: t.span
								message: "Members are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseMemberDecl: t.span, rest]
				}
			}
			at #[my t = Token[has], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid case"
						info: #[
							Info[
								span: t.span
								message: "Cases are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseCaseDecl: t.span, rest]
				}
			}
			at #[Token[init], ...my rest] => return This[parseInitDecl: generics, rest][fatalIfBad: tokens]
			at #[Token[on], ...my rest] => return This[parseOnDecl: generics, rest][fatalIfBad: tokens]
			at #[Token[operator], ...my rest] => return This[parseOperatorDecl: generics, rest][fatalIfBad: tokens]
			at #[my t = Token[deinit], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid deinitializer"
						info: #[
							Info[
								span: t.span
								message: "Deinitializers are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseDeinitDecl: t.span, rest]
				}
			}
			at #[] => return Result[eof: tokens]
			else => return Result[fatal: tokens, Maybe[none]]
		}
	}
	
	
	on [parseGenericParam: span (Span), tokens (Tokens)] (Result[Generic.Param]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}] {
				try {
					my parents = {
						match This[parseTypeParents: rest allowEOL: true] {
							at Result[success: my made, rest = _] => return Maybe[the: made]
							at Result[failure: _, _] => return Maybe[none]
							at my fail => throw fail[fatalIfBad: rest]
						}
					}
					
					my attrs = Generic.Param.Attrs[empty]
					
					while true {
						match rest {
							at #[my i = Token[is], Token[native], my l = Token[lBracket], ...my rest'] {
								match This[parseIsNativeAttr: rest', attrs, i.span | l.span] {
									at Result[success: attrs = _, rest = _] {}
									at my fail => return fail[Result[Generic.Param]]
								}
							}
							
							at #[my i = Token[is], my a = Token[flags], ...rest = _] {
								attrs |= Generic.Param.Attrs[isFlags: i.span | a.span]
							}
							
							at #[my i = Token[is], my a = Token[strong], ...rest = _] {
								attrs |= Generic.Param.Attrs[isStrong: i.span | a.span]
							}
							
							at #[my i = Token[is], my a = Token[uncounted], ...rest = _] {
								attrs |= Generic.Param.Attrs[isUncounted: i.span | a.span]
							}
							
							else => break
						}
					}
					
					my rule = {
						match rest at #[my t = Token[if], ...my rest'] {
							match This[parseGenericRule: rest'] {
								at Result[success: my rule, rest = _] => return Maybe[the: #{t.span, rule}]
								at my fail => throw fail
							}
						} else {
							return Maybe[none]
						}
					}
					my body = {
						match This[nextDeclBody: rest] {
							at Result[success: my made, rest = _] => return Maybe[the: made]
							at Result[failure: _, _] => return Maybe[none]
							at my fail => throw fail[fatalIfBad: rest]
						}
					}
					
					return Result[success: Generic.Param[:span :name :params :parents :attrs :rule :body], rest]
				} catch {
					at my fail (Result[_]) => return fail[Result[Generic.Param]]
				}
			}
			at my fail => return tokens[fatalIfBad: tokens][Result[Generic.Param]]
		}
	}
	
	on [parseGenericRule: tokens (Tokens)] (Result[Generic.Rule]) {
		match This[parseGenericRuleTerm: tokens] {
			at Result[success: my left, my rest] => return This[parseGenericRuleCond: left, rest][updateIfBad: rest]
			at my fail => return fail[updateIfBad: tokens][Result[Generic.Rule]]
		}
	}
	
	on [parseGenericRuleCond: left (Generic.Rule), tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens {
			at #[my op = Token[andAnd], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left and: op.span :right], rest']
				at my fail => return fail
			}
			at #[my op = Token[barBar], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left or: op.span :right], rest']
				at my fail => return fail
			}
			at #[my op = Token[caretCaret], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left xor: op.span :right], rest']
				at my fail => return fail
			}
			at #[my op = Token[bangBang], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left nor: op.span :right], rest']
				at my fail => return fail
			}
			else => return Result[success: left, tokens]
		}
	}
	
	on [parseGenericRuleTerm: tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens {
			at #[my b = Token[bang], my l = Token[lParen], ...my rest] => match This[parseGenericRuleParen: l.span, rest] {
				at Result[success: my right, my rest'] => Generic.Rule[not: b.span :right]
				at my fail => return fail
			}
			at #[my l = Token[lParen], ...my rest] => return This[parseGenericRuleParen: l.span, rest]
			at #[Token[typeName: _] || Token[wildcard], ..._] => match This[parseType: tokens allowEOL: true] {
				at Result[success: my left, my rest'] => match rest' {
					at #[my op = Token[questionEq], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left eq: op.span :right], rest'']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[my op = Token[bangEq], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left ne: op.span :right], rest'']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[my op = Token[of], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left of: op.span :right], rest'']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[] => return Result[eof: tokens]
					else => return Result[failure: tokens, Maybe[the: rest]]
				}
				at my fail => return fail[Result[Generic.Rule]]
			}
			at #[] => return Result[eof: tokens]
			else => return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseGenericRuleParen: begin (Span), tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens at #[Token[rParen], ..._] {
			return Result[fatal: tokens, Maybe[none]]
		}
		
		my rest = tokens
		my level = 1
		
		while level > 0 {
			match rest {
				at #[Token[lSep], ..._] => rest[removeAt: 0]
				at #[Token[lParen], ...rest = _] => level++
				at #[Token[rParen], ...rest = _] => level--
				at #[] => return Result[eof: tokens]
				else => rest++
			}
		}
		
		my leadingOp = {
			match rest {
				at #[Token[andAnd], ...rest = _] => return Maybe[the: 3]
				at #[Token[barBar], ...rest = _] => return Maybe[the: 4]
				at #[Token[caretCaret], ...rest = _] => return Maybe[the: 5]
				at #[Token[bangBang], ...rest = _] => return Maybe[the: 6]
				else => return Maybe[none]
			}
		}
		
		match This[parseGenericRule: rest] {
			at Result[success: my rule, _] if leadingOp? && leadingOp.value != #asm #kind_id rule => return Result[fatal: tokens, Maybe[the: rest[previous]]]
			at Result[success: my rule, #[my r = Token[rParen], ...my rest']] => return Result[success: Generic.Rule[:begin paren: rule end: r.span], rest']
			at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
			at my fail => return fail[fatalIfBad: tokens]
		}
	}
	
	
	on [parseUsePragma: generics (Array[Generic.Param]), span (Span), span' (Span), sym (Str), tokens (Tokens)] (Result[Decl]) {
		return Result[success: Use[:generics :span pragma: Ident[span: span' name: sym]], tokens]
	}
	
	on [parseUseDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeSpec: tokens] {
			at Result[success: my spec, #[Token[label: "from" span: my span'], ...my rest]] => match This[parseType: rest] {
				at Result[success: my type, my rest'] => return Result[success: Use[:generics :span import: spec from: span', type], rest']
				at my fail => return fail[Result[Decl]]
			}
			at Result[success: my spec, my rest] => return Result[success: Use[:generics :span import: spec], rest]
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseAliasDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}, my rest] {
				try {
					match This[parseTypeAnno: rest] {
						at Result[success: my type, my rest'] {
							my attrs = {
								match This[parseAliasDeclAttrs: rest'] {
									at Result[success: my attrs', rest' = _] => return attrs'
									at my fail => throw fail
								}
							}
							my body = {
								match This[nextDeclBody: rest'] {
									at Result[success: my body', rest' = _] => return Maybe[the: body']
									at Result[failure: _, _] => return Maybe[none]
									at my fail => throw fail
								}
							}
							
							return Result[success: Alias[:generics :span :name :params :attrs strong: type :body], rest']
						}
						
						at Result[failure: _, _] {
							my attrs = {
								match This[parseAliasDeclAttrs: rest'] {
									at Result[success: my attrs', rest' = _] => return attrs'
									at my fail => throw fail
								}
							}
							
							match rest {
								at #[Token[eq]] => return Result[eof: tokens]
								at #[Token[eq], ...my rest'] => match This[parseType: rest'] {
									at Result[success: my type, my rest''] {
										return Result[success: Alias[:generics :span :name :params :attrs direct: type], rest'']
									}
									at my fail => throw fail
								}
								else {
									my body = {
										match This[nextDeclBody: rest] {
											at Result[success: my body', rest = _] => return Maybe[the: body']
											at Result[failure: _, _] => return Maybe[none]
											at my fail => throw fail
										}
									}
									
									return Result[success: Alias[:generics :span :name :params :attrs opaque: body], rest]
								}
							}
						}
						
						at my fail => return fail[Result[Decl]]
					}
				} catch {
					at my fail (Result[_]) => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	on [parseAliasDeclAttrs: tokens (Tokens)] (Result[Alias.Attrs]) is inline {
		my attrs = Alias.Attrs[empty]
		
		while true {
			match {
				match tokens {
					at #[my i = Token[is], my a = Token[hidden], ...my rest] {
						return This[parseIsHiddenAttr: rest, attrs, i.span | a.span]
					}
					at #[my i = Token[is], my a = Token[friend], ...my rest] {
						return This[parseIsFriendAttr: rest, attrs, i.span | a.span]
					}
					else => break
				}
			} {
				at Result[success: attrs = _, tokens = _] {}
				at my fail => return fail
			}
		}
		
		return Result[success: attrs, tokens]
	}
	
	
	on [parseModuleDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}, my rest] {
				my parents
				match This[parseTypeParents: rest] {
					at Result[success: my parents', rest = _] => parents = Maybe[the: parents']
					at Result[failure: _, _] => parents = Maybe[none]
					at my fail => return fail
				}
				
				my attrs = Module.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[friend], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[sealed], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[main], ...rest = _] {
							attrs |= Module.Attrs[isMain: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[native], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Module.Attrs[is: i.span | a.span native: Ident[name: sym span: span']]
						}
						
						else => break
					}
				}
				
				match This[nextDeclBody: rest] {
					at Result[success: my body, my rest'] {
						return Result[success: Module[:generics :span :name :params :parents :attrs :body], rest']
					}
					at my fail => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseClassDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}, my rest] {
				my parents
				match This[parseTypeParents: rest] {
					at Result[success: my parents', rest = _] => parents = Maybe[the: parents']
					at Result[failure: _, _] => parents = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				my attrs = Class.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[friend], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[sealed], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], Token[native], my l = Token[lBracket], ...my rest'] {
							match This[parseIsNativeAttr: rest', attrs, i.span | l.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[strong], ...rest = _] {
							attrs |= Class.Attrs[isStrong: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[uncounted], ...rest = _] {
							attrs |= Class.Attrs[isUncounted: i.span | a.span]
						}
						
						else => break
					}
				}
				
				match This[nextDeclBody: rest] {
					at Result[success: my body, my rest'] {
						return Result[success: Class[:generics :span :name :params :parents :attrs :body], rest']
					}
					at my fail => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseProtocolDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}, my rest] {
				my parents
				match This[parseTypeParents: rest] {
					at Result[success: my parents', rest = _] => parents = Maybe[the: parents']
					at Result[failure: _, _] => parents = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				my attrs = Protocol.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[friend], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[sealed], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						else => break
					}
				}
				
				match This[nextDeclBody: rest] {
					at Result[success: my body, my rest'] {
						return Result[success: Protocol[:generics :span :name :params :parents :attrs :body], rest']
					}
					at my fail => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseCategoryDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseType: tokens] {
			at Result[success: my path, my rest] {
				my type
				match rest {
					at #[Token[for], ...my rest'] => match This[parseType: rest'] {
						at Result[success: my type', rest = _] => type = Maybe[the: type']
						at my fail => return fail[Result[Decl]]
					}
					else => type = Maybe[none]
				}
				
				my attrs = Category.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[friend], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						else => break
					}
				}
				
				match This[nextDeclBody: rest] {
					at Result[success: my body, my rest'] {
						return Result[success: Category[:generics :span :path :type :attrs :body], rest']
					}
					at my fail => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseKindDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeDeclName: tokens] {
			at Result[success: #{my name, my params}, my rest] {
				my repr
				match This[parseTypeAnno: rest] {
					at Result[success: my repr', rest = _] => repr = Maybe[the: repr']
					at Result[failure: _, _] => repr = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				my parents
				match This[parseTypeParents: rest] {
					at Result[success: my parents', rest = _] => parents = Maybe[the: parents']
					at Result[failure: _, _] => parents = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				my attrs = Kind.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[friend], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[sealed], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[flags], ...my rest'] {
							attrs |= Kind.Attrs[isFlags: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[strong], ...rest = _] {
							attrs |= Kind.Attrs[isStrong: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[uncounted], ...rest = _] {
							attrs |= Kind.Attrs[isUncounted: i.span | a.span]
						}
						
						else => break
					}
				}
				
				match This[nextDeclBody: rest] {
					at Result[success: my body, my rest'] {
						return Result[success: Kind[:generics :span :name :params :repr :parents :attrs :body], rest']
					}
					at my fail => return fail[Result[Decl]]
				}
			}
			at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
		}
	}
	
	
	on [parseMemberDecl: span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens {
			at #[Token[name: my name span: my span'] = _[asSoftName], ...my rest] {
				my type
				match This[parseTypeAnno: rest] {
					at Result[success: my type', ...rest = _] => type = Maybe[the: type']
					at Result[failure: _, _] => type = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				my attrs = Member.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[static], ...rest = _] {
							attrs |= Member.Attrs[isStatic: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[readonly], ...rest = _] {
							attrs |= Member.Attrs[isReadonly: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[getter], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Member.Attrs[is: i.span | a.span getter: Maybe[the: Ident[name: sym span: span']]]
						}
						at #[my i = Token[is], my a = Token[getter], ...rest = _] {
							attrs |= Member.Attrs[is: i.span | a.span getter: Maybe[none]]
						}
						
						at #[my i = Token[is], my a = Token[setter], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Member.Attrs[is: i.span | a.span setter: Maybe[the: Ident[name: sym span: span']]]
						}
						at #[my i = Token[is], my a = Token[setter], ...rest = _] {
							attrs |= Member.Attrs[is: i.span | a.span setter: Maybe[none]]
						}
						
						at #[my i = Token[is], my a = Token[noinherit], ...rest = _] {
							attrs |= Member.Attrs[isNoinherit: i.span | a.span]
						}
						
						else => break
					}
				}
				
				my value
				match rest {
					at #[Token[eq], ...rest = _] => match This[parseFullExpr: rest] {
						at Result[success: my value', rest = _] => value = Maybe[the: value']
						at my fail => return fail[Result[Decl]]
					}
					else => value = Maybe[none]
				}
				
				return Result[success: Member[:span name: Ident[:name span: span'] :type :attrs :value], rest]
			}
			at #[] => return Result[eof: tokens]
			else => return Result[fatal: tokens, Maybe[none]]
		}
	}
	
	
	on [parseCaseDeclAssoc: tokens (Tokens)] (Result[Message[Type]]) {
		match tokens {
			at #[Token[eqGt], Token[lBracket], ...my rest] => match This[finishTypeMsg: rest] {
				at Result[success: #{my msg, _}, my rest'] => return Result[success: msg, rest']
				at my fail => return fail[fatalIfBad: tokens][Result[Message[Type]]]
			}
			at #[Token[eqGt], ...my rest] => return Result[fatal: tokens, Maybe[the: rest]]
			else => return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseCaseDecl: span (Span), tokens (Tokens)] (Result[Decl]) {
		my case'
		my rest
		
		match tokens {
			at #[Token[name: my name, span: my span'] = _[asAnyName], Token[eqGt], ...rest = _] => match This[parseExpr: rest] {
				at Result[success: my expr, rest = _] {
					case' = Case[name: Ident[:name span: span'] value: Maybe[the: expr]]
				}
				at my fail => return fail[Result[Decl]]
			}
			at #[Token[name: my name, span: my span'] = _[asAnyName], ...rest = _] {
				case' = Case[name: Ident[:name span: span'] value: Maybe[none]]
			}
			at #[Token[lBracket: my begin], ...rest = #[Token[label: _], ..._]] => match This[parseMultiSig: rest] {
				at Result[success: #{my params, my end}, rest = _] {
					my assoc
					match This[parseCaseDeclAssoc: rest] {
						at Result[success: my assoc', rest = _] => assoc = Maybe[the: assoc']
						at Result[failure: _, _] => assoc = Maybe[none]
						at my fail => return fail[Result[Decl]]
					}
					
					case' = Case[tag: Delims[:begin of: Tag[multi: params] :end] :assoc]
				}
				at my fail => return fail[Result[Decl]]
			}
			at #[Token[lBracket: my begin], Token[name: my name span: my span'] = _[asAnyName], Token[rBracket: my end], ...rest = _] {
				my assoc
				match This[parseCaseDeclAssoc: rest] {
					at Result[success: my assoc', rest = _] => assoc = Maybe[the: assoc']
					at Result[failure: _, _] => assoc = Maybe[none]
					at my fail => return fail[Result[Decl]]
				}
				
				case' = Case[tag: Delims[:begin of: Case.Tag[single: Ident[:name span: span']] :end] :assoc]
			}
			at #[] => return Result[eof: tokens]
			else => return Result[fatal: tokens, Maybe[none]]
		}
		
		case'.span = span
		
		match This[parseBlock: rest] {
			at Result[success: my block, rest = _] => case'.init = Maybe[the: block]
			at Result[failure: _, _] => case'.init = Maybe[none]
			at my fail => return fail[Result[Decl]]
		}
		
		return Result[success: case', rest]
	}
	
	
	;== Sigs
	
	on [parseMultiSig: tokens (Tokens)] (Result[Tuple[Multi.Params, Span]]) {
		my rest = tokens
		my params = #[]
		
		while true {
			if params? {
				match rest {
					at #[my r = Token[rBracket], ...my rest'] => return Result[success: #{params, r.span}, rest']
					at #[_[isAnySep], ...my rest'] => rest = rest'
					at #[Token[label: _], ..._] {}
					at #[] => return Result[eof: tokens]
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
			}
			
			match rest {
				;-- Checking for `a: (B)` syntax before `a: a' (B)` syntax is
				;-- probably less expensive than doing it after.
				at #[Token[label: my label span: my span], ...my rest' = #[Token[lParen], ..._]] {
					my label' = Ident[name: label :span]
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[label: label' :type value: Maybe[the: expr]]]
								at my fail => return fail[fatalIfFailed][Result[Tuple[Multi.Params, Span]]]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[label: label' :type value: Maybe[none]]]
						at my fail => return fail[fatalIfBad: tokens][Result[Tuple[Multi.Params, Span]]]
					}
				}
				at #[Token[label: my label span: my span], Token[name: my name span: my span'] = _[asSoftName], ...my rest'] {
					my label' = Ident[name: label :span]
					my name' = Ident[:name span: span']
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[label: label' name: name' :type value: Maybe[the: expr]]]
								at my fail => return fail[fatalIfFailed][Result[Tuple[Multi.Params, Span]]]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[label: label' name: name' :type value: Maybe[none]]]
						at my fail => return fail[fatalIfBad: tokens][Result[Tuple[Multi.Params, Span]]]
					}
				}
				at #[Token[label: _], ...my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at #[Token[lParen], ..._] if params? {
					match This[parseTypeAnno: rest, true] {
						at Result[success: my type, rest = _] => params[add: Multi.Param[:type]]
						at my fail => return fail[fatalIfFailed][Result[Tuple[Multi.Params, Span]]]
					}
				}
				at #[Token[name: my name span: my span] = _[asSoftName], ...my rest'] if params? {
					my name' = Ident[:name :span]
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[name: name' :type value: Maybe[the: expr]]]
								at my fail => return fail[fatalIfFailed][Result[Tuple[Multi.Params, Span]]]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[name: name' :type value: Maybe[none]]]
						at my fail => return fail[fatalIfBad: tokens][Result[Tuple[Multi.Params, Span]]]
					}
				}
				at #[] => return Result[eof: tokens]
				else => return Result[fatal: tokens, Maybe[the: rest]]
			}
		}
	}
	
	
	;== Methods
	
	on [parseMethodDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens at #[my l = Token[lBracket], ...my rest] {
			my begin = l.span
			my kind, my end, match rest {
				at #[Token[label: _], ..._] => match This[parseMultiSig: rest] {
					at Result[success: #{my params, end = _}, rest = _] => kind = Method.Spec[multi: params]
					at my fail => return fail[fatalIfBad: rest][Result[Decl]]
				}
				at #[Token[name: my name span: my span'] = _[asAnyName], my r = Token[rBracket], ...rest = _] {
					kind = Method.Spec[single: Ident[:name span: span']]
					end = r.span
				}
				else => match This[parseType: rest] {
					at Result[success: my type, #[my r = Token[rBracket], ...rest = _]] {
						kind = Method.Spec[cast: type]
						end = r.span
					}
					at Result[success: _, my rest'] => return Result[fatal: rest, Maybe[the: rest']]
					at my fail => return fail[fatalIfBad: rest][Result[Decl]]
				}
			}
			
			my ret, match This[parseTypeAnno: rest] {
				at Result[success: my ret', rest = _] => ret = Maybe[the: ret']
				at Result[failure: _, _] => ret = Maybe[none]
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
			
			my attrs = Method.Attrs[empty]
			
			while true {
				match rest {
					at #[my i = Token[is], my a = Token[static], ...rest = _] {
						attrs |= Method.Attrs[isStatic: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
						match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
							at Result[success: attrs = _, rest = _] {}
							at my fail => return fail[Result[Decl]]
						}
					}
					
					at #[my i = Token[is], my a = Token[main], ...rest = _] {
						attrs |= Method.Attrs[isMain: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[getter], ...rest = _] {
						attrs |= Method.Attrs[isGetter: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[setter], ...rest = _] {
						attrs |= Method.Attrs[isSetter: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[noinherit], ...rest = _] {
						attrs |= Method.Attrs[isNoinherit: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[unordered], ...rest = _] {
						attrs |= Method.Attrs[isUnordered: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[native], Token[litsym: my sym span: my span'], ...rest = _] {
						attrs |= Method.Attrs[is: i.span | a.span native: Maybe[the: Ident[name: sym span: span']]]
					}
					at #[my i = Token[is], my a = Token[native], ...rest = _] {
						attrs |= Method.Attrs[is: i.span | a.span native: Maybe[none]]
					}
					
					at #[my i = Token[is], my a = Token[inline], ...rest = _] {
						attrs |= Method.Attrs[isInline: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[asm], ...rest = _] {
						attrs |= Method.Attrs[isAsm: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[macro], ...rest = _] {
						attrs |= Method.Attrs[isMacro: i.span | a.span]
					}
					
					else => break
				}
			}
			
			my body, match This[parseBlock: rest] {
				at Result[success: my block, rest = _] => body = Maybe[the: block]
				at Result[failure: _, _] => body = Maybe[none]
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
			
			return Result[success: Method[
				:generics
				:span
				spec: Delims[:begin of: kind :end]
				:ret
				:attrs
				:body
			], rest]
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	
	;== Inits
	
	on [parseInitDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens {
			at #[my l = Token[lBracket], ...my rest] {
				my begin = l.span
				my kind, my end, match rest {
					at #[Token[label: _], ..._] => match This[parseMultiSig: rest] {
						at Result[success: #{my params, end = _}, rest = _] => kind = Init.Spec[multi: params]
						at my fail => return fail[fatalIfBad: rest][Result[Decl]]
					}
					at #[Token[name: my name span: my span'] = _[asAnyName], my r = Token[rBracket], ...rest = _] {
						kind = Init.Spec[single: Ident[:name span: span']]
						end = r.span
					}
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
				
				my attrs = Init.Attrs[empty]
				
				while true {
					match rest {
						at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[my i = Token[is], my a = Token[noinherit], ...rest = _] {
							attrs |= Init.Attrs[isNoinherit: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[unordered], ...rest = _] {
							attrs |= Init.Attrs[isUnordered: i.span | a.span]
						}
						
						at #[my i = Token[is], my a = Token[native], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Init.Attrs[is: i.span | a.span native: Maybe[the: Ident[name: sym span: span']]]
						}
						at #[my i = Token[is], my a = Token[native], ...rest = _] {
							attrs |= Init.Attrs[is: i.span | a.span native: Maybe[none]]
						}
						
						at #[my i = Token[is], my a = Token[asm], ...rest = _] {
							attrs |= Init.Attrs[isAsm: i.span | a.span]
						}
						
						else => break
					}
				}
				
				my body, match This[parseBlock: rest] {
					at Result[success: my block, rest = _] => body = Maybe[the: block]
					at Result[failure: _, _] => body = Maybe[none]
					at my fail => return fail[fatalIfBad: rest][Result[Decl]]
				}
				
				return Result[success: Init[
					:generics
					:span
					spec: Delims[:begin of: kind :end]
					:attrs
					:body
				], rest]
			}
			at #[my i = Token[is], my a = Token[static], ...my rest] => match This[parseBlock: rest] {
				at Result[success: my body, my rest'] {
					return Result[success: DefaultInit[
						:span
						attrs: DefaultInit.Attrs[isStatic: i.span | a.span]
						:body
					], rest']
				}
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
			else => match This[parseBlock: tokens] {
				at Result[success: my body, my rest] {
					return Result[success: DefaultInit[
						:span
						attrs: DefaultInit.Attrs[empty]
						:body
					], rest]
				}
				at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
			}
		}
	}
	
	
	;== Operators
	
	on [parseInitDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens at #[Token[litsym: my sym span: my span'], ...my rest] {
			my spec, match rest {
				at #[Token[lBracket], Token[rBracket], ..._] => return Result[fatal: tokens, Maybe[the: rest]] ;@@ TODO: custom error message
				at #[my l = Token[lBracket], Token[name: my name span: my span''] = _[asSoftName], ...my rest'] {
					match This[parseTypeAnno: rest'] {
						at Result[success: my type, #[my r = Token[rBracket], ...rest = _]] {
							spec = Maybe[
								the: Delims[
									begin: l.span
									of: Operator.Spec[name: Ident[:name span: span''] :type]
									end: r.span
								]
							]
						}
						at Result[success: _, my rest''] => return Result[fatal: tokens, Maybe[the: rest'']]
						at my fail => return fail[fatalIfBad: rest'][Result[Decl]]
					}
				}
				else => spec = Maybe[none]
			}
			
			my ret, match This[parseTypeAnno: rest] {
				at Result[success: my ret', rest = _] => ret = Maybe[the: ret']
				at Result[failure: _, _] => ret = Maybe[none]
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
			
			my attrs = Operator.Attrs[empty]
			
			while true {
				match rest {
					at #[my i = Token[is], my a = Token[hidden], ...my rest'] {
						match This[parseIsHiddenAttr: rest', attrs, i.span | a.span] {
							at Result[success: attrs = _, rest = _] {}
							at my fail => return fail[Result[Decl]]
						}
					}
					
					at #[my i = Token[is], my a = Token[noinherit], ...rest = _] {
						attrs |= Operator.Attrs[isNoinherit: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[native], Token[litsym: my sym span: my span''], ...rest = _] {
						attrs |= Operator.Attrs[is: i.span | a.span native: Maybe[the: Ident[name: sym span: span'']]]
					}
					at #[my i = Token[is], my a = Token[native], ...rest = _] {
						attrs |= Operator.Attrs[is: i.span | a.span native: Maybe[none]]
					}
					
					at #[my i = Token[is], my a = Token[inline], ...rest = _] {
						attrs |= Operator.Attrs[isInline: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[asm], ...rest = _] {
						attrs |= Operator.Attrs[isAsm: i.span | a.span]
					}
					
					at #[my i = Token[is], my a = Token[macro], ...rest = _] {
						attrs |= Operator.Attrs[isMacro: i.span | a.span]
					}
					
					else => break
				}
			}
			
			my body, match This[parseBlock: rest] {
				at Result[success: my block, rest = _] => body = Maybe[the: block]
				at Result[failure: _, _] => body = Maybe[none]
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
			
			return Result[success: Operator[
				:generics
				:span
				symbol: Ident[name: sym span: span']
				:spec
				:ret
				:attrs
				:body
			], rest]
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	
	;== Deinits
	
	on [parseDeinitDecl: span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens at #[my i = Token[is], my a = Token[static], ...my rest] {
			match This[parseBlock: rest] {
				at Result[success: my body, my rest'] {
					return Result[success: Deinit[
						:span
						attrs: Deinit.Attrs[isStatic: i.span | a.span]
						:body
					], rest']
				}
				at my fail => return fail[fatalIfBad: rest][Result[Decl]]
			}
		} else {
			match This[parseBlock: tokens] {
				at Result[success: my body, my rest] {
					return Result[success: Deinit[:span attrs: Deinit.Attrs[empty] :body], rest]
				}
				at my fail => return fail[fatalIfBad: tokens][Result[Decl]]
			}
		}
	}
	
	
	;== Types
	
	on [parseTypeParents: tokens (Tokens), allowEOL (Bool) = true] (Result[Tuple[Span, Array[Type]]]) {
		match tokens at #[my o = Token[of], ...my rest] {
			match This[parseType: rest] {
				at Result[success: my type, rest = _] {
					my parents = #[type]
					
					while true {
						match rest at #[Token[comma], ...my rest'] {
							match This[parseType: rest'] {
								at Result[success: my type', rest = _] => parents[add: type']
								at _ if allowEOL => break
								at my fail => return fail[fatalIfBad: rest][Result[Tuple[Span, Array[Type]]]]
							}
						} else {
							break
						}
					}
					
					return Result[success: #{o.span, parents}, rest]
				}
				at my fail => return fail[fatalIfBad: rest][Result[Tuple[Span, Array[Type]]]]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeDeclName: tokens (Tokens)] (Result[Tuple[Ident, Maybe[Type.Args]]]) {
		match tokens at #[Token[typeName: my name span: my span], ...my rest] {
			my name' = Ident[:name :span]
			match This[parseTypeArgs: rest] {
				at Result[success: my params, my rest'] => return Result[success: #{name', Maybe[the: params]}, rest']
				at Result[failure: _, _] => return Result[success: #{name', Maybe[none]}, rest]
				at my fail => return fail[Result[Tuple[Ident, Maybe[Type.Args]]]]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeSpec: tokens (Tokens)] (Result[TypeSpec]) {
		match tokens at #[my l = Token[lHashBracket], ...my rest] {
			my types = #[]
			
			while true {
				match This[parseType: rest] {
					at Result[success: my type, my rest'] {
						types[add: type]
						
						match rest' {
							at #[my r = Token[rBracket], ...my rest''] {
								return Result[success: TypeSpec[begin: l.span :types end: r.span], rest'']
							}
							at #[] || #[_[isAnySep]] => return Result[eof: tokens]
							at #[_[isAnySep], ...rest = _] {}
							else => return Result[fatal: tokens, Maybe[the: rest']]
						}
					}
					at my fail => return fail[Result[TypeSpec]]
				}
			}
		} else {
			match This[parseType: tokens] {
				at Result[success: my type, my rest] => return Result[success: TypeSpec[:type], rest]
				at my fail => return fail[Result[TypeSpec]]
			}
		}
	}
	
	on [parseType: tokens (Tokens), allowWildcard (Bool) = false] (Result[Type]) {
		my rest = tokens
		my leading
		
		match rest at #[my t = Token[wildcard], ...my rest'] {
			match This[parseTypeArgs: rest'] {
				at Result[success: my args, my rest''] {
					if allowWildcard {
						return Result[success: Type[blank: t.span :args], rest'']
					} else {
						return Result[failure: tokens, Maybe[the: rest']]
					}
				}
				at Result[failure: _, _] {
					match rest' {
						at #[Token[dot], ...rest = #[Token[typeName: _], ..._]] {
							leading = #[t.span]
						}
						at #[Token[dot], ...rest = #[Token[wildcard], ..._]] {
							leading = #[t.span]
							
							while true {
								match rest {
									at #[my t' = Token[wildcard], Token[dot], ...rest = _] => leading[add: t'.span]
									at #[Token[wildcard], ..._] => return Result[failure: tokens, Maybe[the: rest]]
									else => break
								}
							}
						}
						else {
							if allowWildcard {
								return Result[success: Type[blank: t.span], rest]
							} else {
								return Result[failure: tokens, Maybe[none]]
							}
						}
					}
				}
				at my fail => return fail[Result[Type]]
			}
		} else {
			leading = #[]
		}
		
		match This[parseTypeSeg: rest] {
			at Result[success: my seg, my rest'] {
				match rest' at #[Token[dot], Token[typeName], ..._] {
					match This[parseTypeSegs: rest'] {
						at Result[success: my segs, my rest''] {
							segs[prepend: seg]
							return Result[success: Type[:leading :segs], rest'']
						}
						at Result[failure: _, _] => return Result[success: Type[:leading segs: #[seg]], rest']
						at my fail => return fail[Result[Type]]
					}
				} else {
					return Result[success: Type[:leading segs: #[seg]], rest']
				}
			}
			at my fail => return fail[Result[Type]]
		}
	}
	
	on [parseTypeSeg: tokens (Tokens)] (Result[Type.Seg]) {
		match tokens at #[Token[typeName: my name span: my span], ...my rest] {
			my name' = Ident[:name :span]
			match This[parseTypeArgs: rest] {
				at Result[success: my args, my rest'] => return Result[success: Type.Seg[name: name' :args], rest']
				at Result[failure: _, _] => return Result[success: Type.Seg[name: name'], rest']
				at my fail => return fail[Result[Type.Seg]]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeSegs: tokens (Tokens)] (Result[Array[Type.Seg]]) {
		match tokens at #[Token[dot], ...my rest] {
			match This[parseTypeSeg] {
				at Result[success: my seg, ...my rest'] => match This[parseTypeSegs: rest'] {
					at Result[success: my segs, my rest''] {
						segs[prepend: seg]
						return Result[success: segs, rest'']
					}
					at Result[failure: _, _] => return Result[success: #[seg], rest']
					at my fail => return fail
				}
				at my fail => return fail[Result[Array[Type.Seg]]]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeArgs: tokens (Tokens)] (Delims[Array[Type]]) {
		match tokens at #[my l = Token[lBracket], ...my rest] {
			my types = #[]
			
			while true {
				match This[parseType: rest, true] {
					at Result[success: my type, my rest'] {
						types[add: type]
						
						match rest' {
							at #[my r = Token[rBracket], ...my rest''] {
								return Result[success: Delims[begin: l.span of: types end: r.span], rest'']
							}
							at #[] || #[_[isAnySep]] => return Result[eof: tokens]
							at #[
								Token[lSep]
								(Token[name: _] || Token[label: _] || Token[punned: _]) = _[asAnyName]
								..._
							] if types.length ?= 1 => return Result[failure: tokens, Maybe[the: rest']]
							at #[_[isAnySep], ...rest = _] {}
							at #[
								(Token[name: _] || Token[label: _] || Token[punned: _]) = _[asAnyName]
								..._
							] if types.length ?= 1 => return Result[failure: tokens, Maybe[the: rest']]
							else => return Result[fatal: tokens, Maybe[the: rest']]
						}
					}
					at my fail => return fail[Result[Delims[Array[Type]]]]
				}
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeAnno: tokens (Tokens), allowWildcard (Bool) = false] (Result[Type]) {
		match tokens {
			at #[Token[lParen]] => return Result[eof: tokens]
			at #[Token[lParen], ...my rest] => match This[parseType: rest, allowWildcard] {
				at Result[success: my type, #[Token[rParen], ...my rest']] => return Result[success: type, rest']
				at Result[success: _, #[]] => return Result[eof: tokens]
				at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at my fail => return fail
			}
			else => return Result[failure: tokens, Maybe[none]]
		}
	}
	
	
	;== Attributes
	
	type T is flags {
		has [is: (Span) hidden: (Maybe[Type])]
	}
	on [parseIsHiddenAttr: tokens (Tokens), attrs (T), span (Span)] (Result[T]) {
		match This[parseType: tokens] {
			at Result[success: my outer, my rest] {
				return Result[success: attrs | T[is: span hidden: Maybe[the: outer]], rest]
			}
			at Result[failure: _, _] {
				return Result[success: attrs | T[is: span hidden: Maybe[none]], tokens]
			}
			at my fail => return fail[Result[T]]
		}
	}
	
	type T is flags {
		has [is: (Span) friend: (TypeSpec)]
	}
	on [parseIsFriendAttr: tokens (Tokens), attrs (T), span (Span)] (Result[T]) {
		match This[parseTypeSpec: tokens] {
			at Result[success: my spec, my rest] {
				return Result[success: attrs | T[is: span friend: spec], rest]
			}
			at my fail => return fail[Result[T]]
		}
	}
	
	type T is flags {
		has [is: (Span) sealed: (Maybe[Type])]
	}
	on [parseIsSealedAttr: tokens (Tokens), attrs (T), span (Span)] (Result[T]) {
		match This[parseType: tokens] {
			at Result[success: my outer, my rest] {
				return Result[success: attrs | T[is: span sealed: Maybe[the: outer]], rest]
			}
			at Result[failure: _, _] {
				return Result[success: attrs | T[is: span sealed: Maybe[none]], tokens]
			}
			at my fail => return fail[Result[T]]
		}
	}
	
	type T is flags {
		has [begin: (Span) isNative: (Array[Tuple[Ident, Expr]]) end: (Span)]
	}
	on [parseIsNativeAttr: tokens (Tokens), attrs (T), span (Span)] (Result[T]) {
		my rest = tokens
		my spec
		
		match rest {
			at #[Token[label: my label span: my span'], ...my rest'] => match This[parseBasicExpr: rest'] {
				at Result[success: my expr, rest = _] => spec = #[#{Ident[name: label span: span'], expr}]
				at my fail => return fail[Result[T]]
			}
			else => return Result[fatal: tokens, Maybe[the: rest]]
		}
		
		while true {
			match rest {
				at #[my r = Token[rBracket], ...rest = _] {
					return Result[success: attrs | T[begin: span isNative: spec end: r.span], rest]
				}
				at #[_[isAnySep], ...rest = _] || _ => match rest {
					at #[Token[label: my label span: my span'], ...rest = _] => match This[parseBasicExpr: rest] {
						at Result[success: my expr, rest = _] => spec[add: #{Ident[name: label span: span'], expr}]
						at my fail => return fail[Result[T]]
					}
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
			}
		}
	}
	
	
	;== Misc
	
	on [trimTokens: tokens (Tokens)] {
		match tokens {
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
				..._
			] {
				tokens[removeAt: 1]
				This[trimTokens: tokens[skip: 1]]
			}
			
			at #[
				Token[lSep]
				Token[rParen] || Token[rBracket] || Token[rBrace]
				..._
			] {
				tokens[removeAt: 0]
				This[trimTokens: tokens[skip: 1]]
			}
			
			at #[Token[str: my segs], ...my rest] {
				for my seg in: segs {
					match seg at StrSegment[code: my code] {
						This[trimTokens: code]
					}
				}
				
				This[trimTokens: rest]
			}
			
			at #[Token[lSep]] => tokens[remove]
			at #[] => return
			else => This[trimTokens: tokens[next]]
		}
	}
}