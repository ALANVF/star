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
		my badTokens = IdentitySet[Token] #[]
		
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
			at #[Token[lBrace: my begin], Token[rBrace: my end], ...my rest] {
				return Result[success: Body[:begin of: #[] :end], rest]
			}
			
			at #[Token[lBrace: my begin], ...my rest] {
				my decls = #[]
				
				while true {
					match This[nextDecl: #[], rest] {
						at Result[success: my decl, my rest'] {
							decls[add: decl]
							
							match rest' {
								at #[Token[rBrace: my end], ...my rest''] => return Result[success: Body[:begin of: decls :end], rest'']
								at #[] || #[_[isAnySep]] => return Result[eof: tokens]
								at #[_[isAnySep], ...rest = _] {}
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
			at #[Token[type: my span], ...my rest] => match This[parseGenericParam: span, rest] {
				at Result[success: my param, #[_[isAnySep], ...my rest']] => return This[nextDecl: generics->[add: decl], rest']
				at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at my fail => return fail[Result[Decl]]
			}
			at #[Token[use: my span], Token[litsym: my sym span: my span'], _[isAnySep], ...my rest] => return This[parseUsePragma: generics, span, span', sym, rest]
			at #[Token[use: my span], ...my rest] => return This[parseUseDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[alias: my span], ...my rest] => return This[parseAliasDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[module: my span], ...my rest] => return This[parseModuleDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[class: my span], ...my rest] => return This[parseClassDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[protocol: my span], ...my rest] => return This[parseProtocolDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[category: my span], ...my rest] => return This[parseCategoryDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[kind: my span], ...my rest] => return This[parseKindDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[my: my span], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid member"
						info: #[
							Info[
								:span
								message: "Members are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseMemberDecl: span, rest]
				}
			}
			at #[Token[has: my span], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid case"
						info: #[
							Info[
								:span
								message: "Cases are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseCaseDecl: span, rest]
				}
			}
			at #[Token[init], ...my rest] => return This[parseInitDecl: generics, rest][fatalIfBad: tokens]
			at #[Token[on], ...my rest] => return This[parseOnDecl: generics, rest][fatalIfBad: tokens]
			at #[Token[operator], ...my rest] => return This[parseOperatorDecl: generics, rest][fatalIfBad: tokens]
			at #[Token[deinit: my span], ...my rest] {
				if generics? {
					return Result[fatalError: Diagnostic[
						severity: Severity.error
						message: "Invalid deinitializer"
						info: #[
							Info[
								:span
								message: "Deinitializers are not allowed to be generic"
								priority: Priority.primary
							]
						]
					]]
				} else {
					return This[parseDeinitDecl: span, rest]
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
							at #[Token[is: my i], Token[native], Token[lBracket: my l], ...my rest'] {
								match This[parseIsNativeAttr: rest', attrs, i | l] {
									at Result[success: attrs = _, rest = _] {}
									at my fail => return fail[Result[Generic.Param]]
								}
							}
							
							at #[Token[is: my i], Token[flags: my a], ...rest = _] {
								attrs |= Generic.Param.Attrs[isFlags: i | a]
							}
							
							at #[Token[is: my i], Token[strong: my a], ...rest = _] {
								attrs |= Generic.Param.Attrs[isStrong: i | a]
							}
							
							at #[Token[is: my i], Token[uncounted: my a], ...rest = _] {
								attrs |= Generic.Param.Attrs[isUncounted: i | a]
							}
							
							else => break
						}
					}
					
					my rule = {
						match rest at #[Token[if: my span'], ...my rest'] {
							match This[parseGenericRule: rest'] {
								at Result[success: my rule, rest = _] => return Maybe[the: #{span', rule}]
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
			at my fail => return tokens[Result[Generic.Param] fatalIfBad: tokens]
		}
	}
	
	on [parseGenericRule: tokens (Tokens)] (Result[Generic.Rule]) {
		match This[parseGenericRuleTerm: tokens] {
			at Result[success: my left, my rest] => return This[parseGenericRuleCond: left, rest][updateIfBad: rest]
			at my fail => return fail[Result[Generic.Rule] updateIfBad: tokens]
		}
	}
	
	on [parseGenericRuleCond: left (Generic.Rule), tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens {
			at #[Token[andAnd: my op], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left and: op :right], rest']
				at my fail => return fail
			}
			at #[Token[barBar: my op], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left or: op :right], rest']
				at my fail => return fail
			}
			at #[Token[caretCaret: my op], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left xor: op :right], rest']
				at my fail => return fail
			}
			at #[Token[bangBang: my op], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left nor: op :right], rest']
				at my fail => return fail
			}
			else => return Result[success: left, tokens]
		}
	}
	
	on [parseGenericRuleTerm: tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens {
			at #[Token[bang: my b], Token[lParen: my l], ...my rest] => match This[parseGenericRuleParen: l, rest] {
				at Result[success: my right, my rest'] => Generic.Rule[not: b :right]
				at my fail => return fail
			}
			at #[Token[lParen: my l], ...my rest] => return This[parseGenericRuleParen: l, rest]
			at #[Token[typeName: _] || Token[wildcard], ..._] => match This[parseType: tokens allowEOL: true] {
				at Result[success: my left, my rest'] => match rest' {
					at #[Token[questionEq: my op], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left eq: op :right], rest'']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[Token[bangEq: my op], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left ne: op :right], rest'']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[Token[of: my op], ...my rest'] => match This[parseType: tokens allowEOL: true] {
						at Result[success: my right, my rest''] => return Result[success: Generic.Rule[:left of: op :right], rest'']
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
			at Result[success: my rule, _] if leadingOp? && leadingOp.value != #asm #kind_id rule => return Result[fatal: tokens, Maybe[the: rest - 1]]
			at Result[success: my rule, #[Token[rParen: my end], ...my rest']] => return Result[success: Generic.Rule[:begin paren: rule :end], rest']
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
		}
	}
	
	on [parseAliasDeclAttrs: tokens (Tokens)] (Result[Alias.Attrs]) is inline {
		my attrs = Alias.Attrs[empty]
		
		while true {
			match {
				match tokens {
					at #[Token[is: my i], Token[hidden: my a], ...my rest] {
						return This[parseIsHiddenAttr: rest, attrs, i | a]
					}
					at #[Token[is: my i], Token[friend: my a], ...my rest] {
						return This[parseIsFriendAttr: rest, attrs, i | a]
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
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[friend: my a], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[sealed: my a], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[main: my a], ...rest = _] {
							attrs |= Module.Attrs[isMain: i | a]
						}
						
						at #[Token[is: my i], Token[native: my a], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Module.Attrs[is: i | a native: Ident[span: span' name: sym]]
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[friend: my a], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[sealed: my a], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[native], Token[lBracket: my l], ...my rest'] {
							match This[parseIsNativeAttr: rest', attrs, i | l] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[strong: my a], ...rest = _] {
							attrs |= Class.Attrs[isStrong: i | a]
						}
						
						at #[Token[is: my i], Token[uncounted: my a], ...rest = _] {
							attrs |= Class.Attrs[isUncounted: i | a]
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[friend: my a], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[sealed: my a], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i | a] {
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
						at #[Token[is: my i], Token[hidden: my i], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[friend: my i], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i | a] {
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[friend: my a], ...my rest'] {
							match This[parseIsFriendAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[sealed: my a], ...my rest'] {
							match This[parseIsSealedAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[flags: my a], ...my rest'] {
							attrs |= Kind.Attrs[isFlags: i | a]
						}
						
						at #[Token[is: my i], Token[strong: my a], ...rest = _] {
							attrs |= Kind.Attrs[isStrong: i | a]
						}
						
						at #[Token[is: my i], Token[uncounted: my a], ...rest = _] {
							attrs |= Kind.Attrs[isUncounted: i | a]
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
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
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
						at #[Token[is: my i], Token[static: my a], ...rest = _] {
							attrs |= Member.Attrs[isStatic: i | a]
						}
						
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[readonly: my a], ...rest = _] {
							attrs |= Member.Attrs[isReadonly: i | a]
						}
						
						at #[Token[is: my i], Token[getter: my a], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Member.Attrs[is: i | a getter: Maybe[the: Ident[span: span' name: sym]]]
						}
						at #[Token[is: my i], Token[getter: my a], ...rest = _] {
							attrs |= Member.Attrs[is: i | a getter: Maybe[none]]
						}
						
						at #[Token[is: my i], Token[setter: my a], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Member.Attrs[is: i | a setter: Maybe[the: Ident[span: span' name: sym]]]
						}
						at #[Token[is: my i], Token[setter: my a], ...rest = _] {
							attrs |= Member.Attrs[is: i | a setter: Maybe[none]]
						}
						
						at #[Token[is: my i], Token[noinherit: my a], ...rest = _] {
							attrs |= Member.Attrs[isNoinherit: i | a]
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
				
				return Result[success: Member[:span name: Ident[span: span' :name] :type :attrs :value], rest]
			}
			at #[] => return Result[eof: tokens]
			else => return Result[fatal: tokens, Maybe[none]]
		}
	}
	
	
	on [parseCaseDeclAssoc: tokens (Tokens)] (Result[Message[Type]]) {
		match tokens {
			at #[Token[eqGt], Token[lBracket], ...my rest] => match This[finishTypeMsg: rest] {
				at Result[success: #{my msg, _}, my rest'] => return Result[success: msg, rest']
				at my fail => return fail[Result[Message[Type]] fatalIfBad: tokens]
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
					case' = Case[name: Ident[span: span' :name] value: Maybe[the: expr]]
				}
				at my fail => return fail[Result[Decl]]
			}
			at #[Token[name: my name, span: my span'] = _[asAnyName], ...rest = _] {
				case' = Case[name: Ident[span: span' :name] value: Maybe[none]]
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
				
				case' = Case[tag: Delims[:begin of: Case.Tag[single: Ident[span: span' :name]] :end] :assoc]
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
					at #[Token[rBracket: my r], ...my rest'] => return Result[success: #{params, r}, rest']
					at #[_[isAnySep], ...rest = _] {}
					at #[Token[label: _], ..._] {}
					at #[] => return Result[eof: tokens]
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
			}
			
			match rest {
				;-- Checking for `a: (B)` syntax before `a: a' (B)` syntax is
				;-- probably less expensive than doing it after.
				at #[Token[label: my label span: my span], ...my rest' = #[Token[lParen], ..._]] {
					my label' = Ident[:span name: label]
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[label: label' :type value: Maybe[the: expr]]]
								at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfFailed]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[label: label' :type value: Maybe[none]]]
						at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfBad: tokens]
					}
				}
				at #[Token[label: my label span: my span], Token[name: my name span: my span'] = _[asSoftName], ...my rest'] {
					my label' = Ident[:span name: label]
					my name' = Ident[span: span' :name]
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[label: label' name: name' :type value: Maybe[the: expr]]]
								at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfFailed]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[label: label' name: name' :type value: Maybe[none]]]
						at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfBad: tokens]
					}
				}
				at #[Token[label: _], ...my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at #[Token[lParen], ..._] if params? {
					match This[parseTypeAnno: rest, true] {
						at Result[success: my type, rest = _] => params[add: Multi.Param[:type]]
						at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfFailed]
					}
				}
				at #[Token[name: my name span: my span] = _[asSoftName], ...my rest'] if params? {
					my name' = Ident[:span :name]
					
					match This[parseTypeAnno: rest', true] {
						at Result[success: my type, #[Token[eq], ...my rest'']] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] => params[add: Multi.Param[name: name' :type value: Maybe[the: expr]]]
								at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfFailed]
							}
						}
						at Result[success: my type, rest = _] => params[add: Multi.Param[name: name' :type value: Maybe[none]]]
						at my fail => return fail[Result[Tuple[Multi.Params, Span]] fatalIfBad: tokens]
					}
				}
				at #[] => return Result[eof: tokens]
				else => return Result[fatal: tokens, Maybe[the: rest]]
			}
		}
	}
	
	
	;== Methods
	
	on [parseMethodDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens at #[Token[lBracket: my begin], ...my rest] {
			my kind, my end, match rest {
				at #[Token[label: _], ..._] => match This[parseMultiSig: rest] {
					at Result[success: #{my params, end = _}, rest = _] => kind = Method.Spec[multi: params]
					at my fail => return fail[Result[Decl] fatalIfBad: rest]
				}
				at #[Token[name: my name span: my span'] = _[asAnyName], Token[rBracket: end = _], ...rest = _] {
					kind = Method.Spec[single: Ident[span: span' :name]]
				}
				else => match This[parseType: rest] {
					at Result[success: my type, #[Token[rBracket: end = _], ...rest = _]] {
						kind = Method.Spec[cast: type]
					}
					at Result[success: _, my rest'] => return Result[fatal: rest, Maybe[the: rest']]
					at my fail => return fail[Result[Decl] fatalIfBad: rest]
				}
			}
			
			my ret, match This[parseTypeAnno: rest] {
				at Result[success: my ret', rest = _] => ret = Maybe[the: ret']
				at Result[failure: _, _] => ret = Maybe[none]
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
			
			my attrs = Method.Attrs[empty]
			
			while true {
				match rest {
					at #[Token[is: my i], Token[static: my a], ...rest = _] {
						attrs |= Method.Attrs[isStatic: i | a]
					}
					
					at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
						match This[parseIsHiddenAttr: rest', attrs, i | a] {
							at Result[success: attrs = _, rest = _] {}
							at my fail => return fail[Result[Decl]]
						}
					}
					
					at #[Token[is: my i], Token[main: my a], ...rest = _] {
						attrs |= Method.Attrs[isMain: i | a]
					}
					
					at #[Token[is: my i], Token[getter: my a], ...rest = _] {
						attrs |= Method.Attrs[isGetter: i | a]
					}
					
					at #[Token[is: my i], Token[setter: my a], ...rest = _] {
						attrs |= Method.Attrs[isSetter: i | a]
					}
					
					at #[Token[is: my i], Token[noinherit: my a], ...rest = _] {
						attrs |= Method.Attrs[isNoinherit: i | a]
					}
					
					at #[Token[is: my i], Token[unordered: my a], ...rest = _] {
						attrs |= Method.Attrs[isUnordered: i | a]
					}
					
					at #[Token[is: my i], Token[native: my a], Token[litsym: my sym span: my span'], ...rest = _] {
						attrs |= Method.Attrs[is: i | a native: Maybe[the: Ident[span: span' name: sym]]]
					}
					at #[Token[is: my i], Token[native: my a], ...rest = _] {
						attrs |= Method.Attrs[is: i | a native: Maybe[none]]
					}
					
					at #[Token[is: my i], Token[inline: my a], ...rest = _] {
						attrs |= Method.Attrs[isInline: i | a]
					}
					
					at #[Token[is: my i], Token[asm: my a], ...rest = _] {
						attrs |= Method.Attrs[isAsm: i | a]
					}
					
					at #[Token[is: my i], Token[macro: my a], ...rest = _] {
						attrs |= Method.Attrs[isMacro: i | a]
					}
					
					else => break
				}
			}
			
			my body, match This[parseBlock: rest] {
				at Result[success: my block, rest = _] => body = Maybe[the: block]
				at Result[failure: _, _] => body = Maybe[none]
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
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
			at #[Token[lBracket: my begin], ...my rest] {
				my kind, my end, match rest {
					at #[Token[label: _], ..._] => match This[parseMultiSig: rest] {
						at Result[success: #{my params, end = _}, rest = _] => kind = Init.Spec[multi: params]
						at my fail => return fail[Result[Decl] fatalIfBad: rest]
					}
					at #[Token[name: my name span: my span'] = _[asAnyName], Token[rBracket: end = _], ...rest = _] {
						kind = Init.Spec[single: Ident[span: span' :name]]
					}
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
				
				my attrs = Init.Attrs[empty]
				
				while true {
					match rest {
						at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
							match This[parseIsHiddenAttr: rest', attrs, i | a] {
								at Result[success: attrs = _, rest = _] {}
								at my fail => return fail[Result[Decl]]
							}
						}
						
						at #[Token[is: my i], Token[noinherit: my a], ...rest = _] {
							attrs |= Init.Attrs[isNoinherit: i | a]
						}
						
						at #[Token[is: my i], Token[unordered: my a], ...rest = _] {
							attrs |= Init.Attrs[isUnordered: i | a]
						}
						
						at #[Token[is: my i], Token[native: my a], Token[litsym: my sym span: my span'], ...rest = _] {
							attrs |= Init.Attrs[is: i | a native: Maybe[the: Ident[span: span' name: sym]]]
						}
						at #[Token[is: my i], Token[native: my a], ...rest = _] {
							attrs |= Init.Attrs[is: i | a native: Maybe[none]]
						}
						
						at #[Token[is: my i], Token[asm: my a], ...rest = _] {
							attrs |= Init.Attrs[isAsm: i | a]
						}
						
						else => break
					}
				}
				
				my body, match This[parseBlock: rest] {
					at Result[success: my block, rest = _] => body = Maybe[the: block]
					at Result[failure: _, _] => body = Maybe[none]
					at my fail => return fail[Result[Decl] fatalIfBad: rest]
				}
				
				return Result[success: Init[
					:generics
					:span
					spec: Delims[:begin of: kind :end]
					:attrs
					:body
				], rest]
			}
			at #[Token[is: my i], Token[static: my a], ...my rest] => match This[parseBlock: rest] {
				at Result[success: my body, my rest'] {
					return Result[success: DefaultInit[
						:span
						attrs: DefaultInit.Attrs[isStatic: i | a]
						:body
					], rest']
				}
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
			else => match This[parseBlock: tokens] {
				at Result[success: my body, my rest] {
					return Result[success: DefaultInit[
						:span
						attrs: DefaultInit.Attrs[empty]
						:body
					], rest]
				}
				at my fail => return fail[Result[Decl] fatalIfBad: tokens]
			}
		}
	}
	
	
	;== Operators
	
	on [parseInitDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match tokens at #[Token[litsym: my sym span: my span'], ...my rest] {
			my spec, match rest {
				at #[Token[lBracket], Token[rBracket], ..._] => return Result[fatal: tokens, Maybe[the: rest]] ;@@ TODO: custom error message
				at #[Token[lBracket: my begin], Token[name: my name span: my span''] = _[asSoftName], ...my rest'] {
					match This[parseTypeAnno: rest'] {
						at Result[success: my type, #[Token[rBracket: my end], ...rest = _]] {
							spec = Maybe[
								the: Delims[
									:begin
									of: Operator.Spec[name: Ident[span: span'' :name] :type]
									:end
								]
							]
						}
						at Result[success: _, my rest''] => return Result[fatal: tokens, Maybe[the: rest'']]
						at my fail => return fail[Result[Decl] fatalIfBad: rest']
					}
				}
				else => spec = Maybe[none]
			}
			
			my ret, match This[parseTypeAnno: rest] {
				at Result[success: my ret', rest = _] => ret = Maybe[the: ret']
				at Result[failure: _, _] => ret = Maybe[none]
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
			
			my attrs = Operator.Attrs[empty]
			
			while true {
				match rest {
					at #[Token[is: my i], Token[hidden: my a], ...my rest'] {
						match This[parseIsHiddenAttr: rest', attrs, i | a] {
							at Result[success: attrs = _, rest = _] {}
							at my fail => return fail[Result[Decl]]
						}
					}
					
					at #[Token[is: my i], Token[noinherit: my a], ...rest = _] {
						attrs |= Operator.Attrs[isNoinherit: i | a]
					}
					
					at #[Token[is: my i], Token[native: my a], Token[litsym: my sym span: my span''], ...rest = _] {
						attrs |= Operator.Attrs[is: i | a native: Maybe[the: Ident[span: span'' name: sym]]]
					}
					at #[Token[is: my i], Token[native: my a], ...rest = _] {
						attrs |= Operator.Attrs[is: i | a native: Maybe[none]]
					}
					
					at #[Token[is: my i], Token[inline: my a], ...rest = _] {
						attrs |= Operator.Attrs[isInline: i | a]
					}
					
					at #[Token[is: my i], Token[asm: my a], ...rest = _] {
						attrs |= Operator.Attrs[isAsm: i | a]
					}
					
					at #[Token[is: my i], Token[macro: my a], ...rest = _] {
						attrs |= Operator.Attrs[isMacro: i | a]
					}
					
					else => break
				}
			}
			
			my body, match This[parseBlock: rest] {
				at Result[success: my block, rest = _] => body = Maybe[the: block]
				at Result[failure: _, _] => body = Maybe[none]
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
			
			return Result[success: Operator[
				:generics
				:span
				symbol: Ident[span: span' name: sym]
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
		match tokens at #[Token[is: my i], Token[static: my a], ...my rest] {
			match This[parseBlock: rest] {
				at Result[success: my body, my rest'] {
					return Result[success: Deinit[
						:span
						attrs: Deinit.Attrs[isStatic: i | a]
						:body
					], rest']
				}
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
		} else {
			match This[parseBlock: tokens] {
				at Result[success: my body, my rest] {
					return Result[success: Deinit[:span attrs: Deinit.Attrs[empty] :body], rest]
				}
				at my fail => return fail[Result[Decl] fatalIfBad: tokens]
			}
		}
	}
	
	
	;== Types
	
	on [parseTypeParents: tokens (Tokens), allowEOL (Bool) = true] (Result[Tuple[Span, Array[Type]]]) {
		match tokens at #[Token[of: my span], ...my rest] {
			match This[parseType: rest] {
				at Result[success: my type, rest = _] {
					my parents = #[type]
					
					while true {
						match rest at #[Token[comma], ...my rest'] {
							match This[parseType: rest'] {
								at Result[success: my type', rest = _] => parents[add: type']
								at _ if allowEOL => break
								at my fail => return fail[Result[Tuple[Span, Array[Type]]] fatalIfBad: rest]
							}
						} else {
							break
						}
					}
					
					return Result[success: #{span, parents}, rest]
				}
				at my fail => return fail[Result[Tuple[Span, Array[Type]]] fatalIfBad: rest]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeDeclName: tokens (Tokens)] (Result[Tuple[Ident, Maybe[Type.Args]]]) {
		match tokens at #[Token[typeName: my name span: my span], ...my rest] {
			my name' = Ident[:span :name]
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
		match tokens at #[Token[lHashBracket: my begin], ...my rest] {
			my types = #[]
			
			while true {
				match This[parseType: rest] {
					at Result[success: my type, my rest'] {
						types[add: type]
						
						match rest' {
							at #[Token[rBracket: my end], ...my rest''] {
								return Result[success: TypeSpec[:begin :types :end], rest'']
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
		
		match rest at #[Token[wildcard: my span], ...my rest'] {
			match This[parseTypeArgs: rest'] {
				at Result[success: my args, my rest''] {
					if allowWildcard {
						return Result[success: Type[blank: span :args], rest'']
					} else {
						return Result[failure: tokens, Maybe[the: rest']]
					}
				}
				at Result[failure: _, _] {
					match rest' {
						at #[Token[dot], ...rest = #[Token[typeName: _], ..._]] {
							leading = #[span]
						}
						at #[Token[dot], ...rest = #[Token[wildcard], ..._]] {
							leading = #[span]
							
							while true {
								match rest {
									at #[Token[wildcard: my span'], Token[dot], ...rest = _] => leading[add: span']
									at #[Token[wildcard], ..._] => return Result[failure: tokens, Maybe[the: rest]]
									else => break
								}
							}
						}
						else {
							if allowWildcard {
								return Result[success: Type[blank: span], rest]
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
			my name' = Ident[:span :name]
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
		match tokens at #[Token[lBracket: my begin], ...my rest] {
			my types = #[]
			
			while true {
				match This[parseType: rest, true] {
					at Result[success: my type, my rest'] {
						types[add: type]
						
						match rest' {
							at #[Token[rBracket: my end], ...my rest''] {
								return Result[success: Delims[:begin of: types :end], rest'']
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
				at Result[success: my expr, rest = _] => spec = #[#{Ident[span: span' name: label], expr}]
				at my fail => return fail[Result[T]]
			}
			else => return Result[fatal: tokens, Maybe[the: rest]]
		}
		
		while true {
			match rest {
				at #[Token[rBracket: my end], ...rest = _] {
					return Result[success: attrs | T[begin: span isNative: spec :end], rest]
				}
				at #[_[isAnySep], ...rest = _] || _ => match rest {
					at #[Token[label: my label span: my span'], ...rest = _] => match This[parseBasicExpr: rest] {
						at Result[success: my expr, rest = _] => spec[add: #{Ident[span: span' name: label], expr}]
						at my fail => return fail[Result[T]]
					}
					else => return Result[fatal: tokens, Maybe[the: rest]]
				}
			}
		}
	}
	
	
	;== Statements
	
	on [parseBlock: tokens (Tokens)] (Result[Block]) {
		match tokens {
			at #[Token[lBrace: my begin], Token[rBrace: my end], ...my rest] {
				return Result[success: Block[:begin stmts: #[] :end], rest]
			}
			at #[Token[lBrace: my begin], ...my rest] {
				my stmts = #[]
				
				while true {
					match This[parseStmt: rest] {
						at Result[success: my stmt, my rest'] {
							stmts[add: stmt]
							
							match rest' {
								at #[Token[rBrace: my end], ...my rest''] => return Result[success: Block[:begin :stmts :end], rest'']
								at #[] || #[_[isAnySep]] => return Result[eof: tokens]
								at #[_[isAnySep], ...rest = _] {}
								else => return Result[fatal: rest, Maybe[the: rest']]
							}
						}
						at my fail => return fail[Result[Block] fatalIfBad: rest]
					}
				}
			}
			else => return Result[failure: tokens, Maybe[none]]
		}
	}
	
	
	on [parseStmt: tokens (Tokens)] (Result[Stmt]) {
		match tokens {
			at #[
				Token[my: my span]
				Token[name: my name span: my span'] = _[asSoftName]
				...my rest
			] {
				my type
				match This[parseTypeAnno: rest] {
					at Result[success: my t, rest = _] => type = Maybe[the: t]
					at Result[failure: _, _] => type = Maybe[none]
					at my fail => return fail[Result[Stmt]]
				}
				
				my value
				match rest at #[Token[eq], ...my rest'] {
					match This[parseFullExpr: rest'] {
						at Result[success: my expr, rest = _] => value = Maybe[the: expr]
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
				} else {
					value = Maybe[none]
				}
				
				return Result[
					success: Stmt[my: span name: Ident[span: span' :name] :type :value],
					rest
				]
			}
			
			at #[Token[if: my span], ...my rest] => match This[parseExpr: rest] {
				at Result[success: my cond, my rest'] => match This[parseBlock: rest'] {
					at Result[success: my then, my rest''] {
						my others = #[]
						while true {
							match rest'' at #[Token[orif: my span'], ...my rest'''] {
								match This[parseExpr: rest'''] {
									at Result[success: my cond', my rest''''] => match This[parseBlock: rest''''] {
										at Result[success: my block, rest'' = _] {
											others[add: Stmt.OrIf[span: span' cond: cond' :block]]
										}
										at my fail => return fail[Result[Stmt]]
									}
									at my fail => return fail[Result[Stmt]]
								}
							} else {
								break
							}
						}
						
						my else'
						match rest'' at #[Token[else: my span'], ...my rest'''] {
							match This[parseBlock: rest'''] {
								at Result[success: my block, rest'' = _] => else' = Maybe[the: #{span', block}]
								at my fail => return fail[Result[Stmt]]
							}
						} else {
							else' = Maybe[none]
						}
						
						return Result[
							success: Stmt[if: span, cond :then :others else: else'],
							rest''
						]
					}
					at my fail => return fail[Result[Stmt]]
				}
				at my fail => return fail[Result[Stmt]]
			}
			
			at #[Token[case: my case], Token[lBrace: my begin], ...my rest] {
				my cases = #[]
				
				while true {
					match rest {
						at #[Token[at: my span], ...my rest'] => match This[parseCaseAtStmt: span, rest'] {
							at Result[success: my case', my rest''] {
								cases[add: case']
								
								match rest'' {
									at #[Token[rBrace: my end], ...my rest'''] {
										return Result[
											success: Stmt[case: span :cases else: Maybe[none]],
											rest'''
										]
									}
									at #[] || #[_[isAnySep]] => return Result[eof: tokens]
									at #[_[isAnySep], rest = _] {}
									else => return Result[fatal: tokens, Maybe[the: rest'']]
								}
							}
							at my fail => return fail[Result[Stmt] fatalIfFailed]
						}
						at #[Token[else: my span], ...my rest'] => match This[parseThenStmt: rest'] {
							at Result[success: my else', #[Token[rBrace], ...my rest'']] {
								return Result[
									success: Stmt[:case :cases else: Maybe[the: #{span, else'}]],
									rest''
								]
							}
							at Result[success: _, my rest''] => return Result[fatal: tokens, Maybe[the: rest'']]
							at my fail => return fail[Result[Stmmt] fatalIfFailed]
						}
						at #[Token[rBrace: my end], ...my rest'] {
							return Result[
								success: Stmt[:case :cases else: Maybe[none]],
								rest'
							]
						}
						else => return Result[fatal: tokens, Maybe[the: rest]]
					}
				}
			}
			
			at #[Token[match: my match], ...my rest] => match This[parseExpr: rest] {
				at Result[success: my value, #[Token[lBrace: my begin], ...my rest']] {
					my cases = #[]
					
					while true {
						match rest' {
							at #[Token[at: my span], ...my rest''] => match This[parsePatternAtStmt: span, rest'] {
								at Result[success: my case', my rest''] {
									cases[add: case']
									
									match rest'' {
										at #[Token[rBrace], ...my rest'''] {
											return Result[
												success: Stmt[:match :value :cases else: Maybe[none]],
												rest'''
											]
										}
										at #[] || #[_[isAnySep]] => return Result[eof: tokens]
										at #[_[isAnySep], rest' = _] {}
										else => return Result[fatal: tokens, Maybe[the: rest'']]
									}
								}
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[else: my span], ...my rest''] => match This[parseThenStmt: rest''] {
								at Result[success: my else', #[Token[rBrace], ...my rest''']] {
									return Result[
										success: Stmt[:match :value :cases else: Maybe[the: #{span, else'}]],
										rest'''
									]
								}
								at Result[success: _, my rest'''] => return Result[fatal: tokens, Maybe[the: rest''']]
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[rBrace], ...my rest''] {
								return Result[
									success: Stmt[:match :value :cases else: Maybe[none]],
									rest''
								]
							}
							else => return Result[fatal: tokens, Maybe[the: rest']]
						}
					}
				}
				
				at Result[success: my value, #[Token[at: my at], ...my rest']] => match This[parsePattern: rest'] {
					at Result[success: my pattern, my rest''] {
						my cond
						match rest'' at #[Token[if: my span], ...my rest'''] {
							match This[parseExpr: rest'''] {
								at Result[success: my cond', rest'' = _] => cond = Maybe[the: #{span, cond'}]
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
						} else {
							cond = Maybe[none]
						}
						
						match This[parseBlock: my rest''] {
							at Result[success: my then, #[Token[else: my span], ...my rest''']] => match This[parseBlock: rest'''] {
								at Result[success: my else', my rest''''] {
									return Result[
										success: Stmt[:match :value :at, pattern if: cond :then else: Maybe[the: #{span, else'}]],
										rest''''
									]
								}
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at Result[success: my then, my rest'''] {
								return Result[
									success: Stmt[:match :value :at, pattern if: cond :then else: Maybe[none]],
									rest'''
								]
							}
							at my fail => return fail[Result[Stmt] fatalIfFailed]
						}
					}
				}
				
				at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[Token[while: my while], ...my rest] => match This[parseExpr: rest] {
				at Result[success: my cond, my rest'] => match This[parseBlock: rest'] {
					at Result[success: my block, my rest''] => return Result[success: Stmt[:while, cond do: block], rest'']
					at my fail => return fail[Result[Stmt] fatalIfFailed]
				}
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[Token[do: my do], ...my rest] => match This[parseBlock: rest] {
				at Result[success: my block, #[Token[while: my while], ...my rest']] => match This[parseExpr: rest'] {
					at Result[success: my cond, my rest''] => return Result[success: Stmt[:do, block :while, cond], rest'']
					at my fail => return fail[Result[Stmt] fatalIfFailed]
				}
				at Result[success: my block, my rest'] => return Result[success: Stmt[:do, block], rest']
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[Token[for: my span], ...my rest] => match this[parsePattern: rest] {
				at Result[success: my var, my rest'] => match rest' {
					at #[Token[label: "from" span: my span'], ...my rest''] {
						return This[parseLoopRange: span, var, Stmt.Loop.Start[from] -> span = span']
					}
					at #[Token[label: "after" span: my span'], ...my rest''] {
						return This[parseLoopRange: span, var, Stmt.Loop.Start[after] -> span = span']
					}
					at #[Token[comma], ...my rest''] => match This[parsePattern: rest''] {
						at Result[success: my var', my rest'''] => return This[parseLoopIn: span, var, Maybe[the: var'], rest''']
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
					else => return This[parseLoopIn: span, var, Maybe[none], rest']
				}
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[Token[return: my return], ...my rest] => match This[parseFullExpr: rest] {
				at Result[success: my value, my rest'] => return Result[success: Stmt[:return :value], rest']
				at Result[failure: _, _] => return Result[success: Stmt[:return], rest]
				at my fail => return fail[Result[Stmt]]
			}
			
			at #[Token[break: my break], ...my rest] => match rest {
				at #[Token[int: my int exp: my exp span: my span], ...my rest'] {
					match exp at Maybe[the: my exp'] {
						return Result[success: Stmt[:break, depth: span, "\(int)e\(exp')"[Int]], rest']
					} else {
						return Result[success: Stmt[:break, depth: span, int[Int]], rest']
					}
				}
				else => return Result[success: Stmt[:break]]
			}
			
			at #[Token[next: my next], ...my rest] => match rest {
				at #[Token[int: my int exp: my exp span: my span], ...my rest'] {
					match exp at Maybe[the: my exp'] {
						return Result[success: Stmt[:next, depth: span, "\(int)e\(exp')"[Int]], rest']
					} else {
						return Result[success: Stmt[:next, depth: span, int[Int]], rest']
					}
				}
				else => return Result[success: Stmt[:next]]
			}
			
			at #[Token[throw: my throw], ...my rest] => match This[parseFullExpr: rest] {
				at Result[success: my value, my rest'] => return Result[success: Stmt[:throw, value], rest']
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[Token[try: my try], ...my rest] => match This[parseBlock: rest] {
				at Result[success: my block, #[Token[catch: my catch], Token[lBrace], ...my rest']] {
					my cases = #[]
					
					while true {
						match rest' {
							at #[Token[at: my span], ...my rest''] => match This[parsePatternAtStmt: span, rest'] {
								at Result[success: my case', my rest''] {
									cases[add: case']
									
									match rest'' {
										at #[Token[rBrace], ...my rest'''] {
											return Result[
												success: Stmt[:try, block :catch :cases else: Maybe[none]],
												rest'''
											]
										}
										at #[] || #[_[isAnySep]] => return Result[eof: tokens]
										at #[_[isAnySep], rest' = _] {}
										else => return Result[fatal: tokens, Maybe[the: rest'']]
									}
								}
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[else: my span], ...my rest''] => match This[parseThenStmt: rest''] {
								at Result[success: my else', #[Token[rBrace], ...my rest''']] {
									return Result[
										success: Stmt[:try, block :catch :cases else: Maybe[the: #{span, else'}]],
										rest'''
									]
								}
								at Result[success: _, my rest'''] => return Result[fatal: tokens, Maybe[the: rest''']]
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[rBrace], ...my rest''] {
								return Result[
									success: Stmt[:try, block :catch :cases else: Maybe[none]],
									rest''
								]
							}
							else => return Result[fatal: tokens, Maybe[the: rest']]
						}
					}
				}
				at Result[success: _, #[Token[catch], ...my rest'] || my rest'] {
					return Result[fatal: tokens, Maybe[the: rest']]
				}
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
			
			at #[] => return Result[eof: tokens]
			else => match This[parseFullExpr: tokens] {
				at Result[success: my expr, my rest] => return Result[success: Stmt[:expr], rest]
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
		}
	}
	
	
	on [parseThenStmt: tokens (Tokens)] (Result[Stmt.Then]) {
		match tokens at #[Token[eqGt: my span], ...my rest] {
			match This[parseStmt: rest] {
				at Result[success: my stmt, my rest'] => return Result[success: Stmt.Then[stmt: span, stmt], rest']
				at my fail => return fail[Result[Stmt.Then] fatalIfFailed]
			}
		} else {
			match This[parseBlock: tokens] {
				at Result[success: my block, my rest] => return Result[success: Stmt.Then[:block], rest]
				at my fail => return fail[Result[Stmt.Then] fatalIfBad: tokens]
			}
		}
	}
	
	on [parseCaseAtStmt: span (Span), tokens (Tokens)] (Result[Stmt.CaseAt]) {
		match This[parseExpr: tokens] {
			at Result[success: my cond, my rest] => match This[parseThenStmt: rest] {
				at Result[success: my then, my rest'] => return Result[success: Stmt.CaseAt[:span :cond :then], rest']
				at my fail => return fail[Result[Stmt.CaseAt] fatalIfFailed]
			}
			at my fail => return fail[Result[Stmt.CaseAt] fatalIfFailed]
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