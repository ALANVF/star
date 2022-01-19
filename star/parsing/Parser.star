use Lexer
use Info from: Diagnostic
use Priority from: Info
use Decl
use Message
use Cascade
use Then from: Stmt

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
		my errors = Array[Diagnostic] #[]
		my decls = Array[Decl] #[]
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
			
			match This[nextDecl: Array[Generic.Param] #[], tokens] {
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
					my last = end?[yes: end[first] no: begin[last]]
					
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
					match This[nextDecl: Array[Generic.Param] #[], rest] {
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
				at Result[success: my param, #[_[isAnySep], ...my rest']] => return This[nextDecl: generics->[add: param], rest']
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
			at #[Token[init: my span], ...my rest] => return This[parseInitDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[on: my span], ...my rest] => return This[parseMethodDecl: generics, span, rest][fatalIfBad: tokens]
			at #[Token[operator: my span], ...my rest] => return This[parseOperatorDecl: generics, span, rest][fatalIfBad: tokens]
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
			at Result[success: #{my name, my params}, my rest] {
				try {
					my parents = {
						match This[parseTypeParents: rest, true] {
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
			at my fail => return fail[Result[Generic.Param] fatalIfBad: tokens]
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
			at #[Token[andAnd: my and], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left :and :right], rest']
				at my fail => return fail
			}
			at #[Token[barBar: my or], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left :or :right], rest']
				at my fail => return fail
			}
			at #[Token[caretCaret: my xor], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left :xor :right], rest']
				at my fail => return fail
			}
			at #[Token[bangBang: my nor], ...my rest] => match This[parseGenericRuleTerm: rest] {
				at Result[success: my right, my rest'] => return This[parseGenericRuleCond: Generic.Rule[:left :nor :right], rest']
				at my fail => return fail
			}
			else => return Result[success: left, tokens]
		}
	}
	
	alias GenericOpChain is hidden = Array[Tuple[Span, Type]]
	on [parseGenericOpChain: tokens (Tokens), op (Token)] (Result[Array[Tuple[Span, Type]]]) is inline {
		my chain = #[]
		
		while true {
			match tokens at #[my op', ...my rest] if #asm (#kind_id op' ?= #kind_id op) {
				match This[parseType: rest, true] {
					at Result[success: my right, tokens = _] => chain[add: #{op'.span, right}]
					at my fail => return fail[Result[GenericCmpChain]]
				}
			} else {
				break
			}
		}
		
		return Result[success: chain, tokens]
	}
	
	alias GenericCmpChain is hidden = Array[Tuple[Span, Generic.Cmp, Type]]
	on [parseGenericCmpChain: tokens (Tokens)] (Result[GenericCmpChain]) is inline {
		my chain = #[]
		
		while true {
			my span, my op = {
				match tokens {
					at #[Token[lt: span = _], ...tokens = _] => return Generic.Cmp.lt
					at #[Token[ltEq: span = _], ...tokens = _] => return Generic.Cmp.le
					at #[Token[gt: span = _], ...tokens = _] => return Generic.Cmp.gt
					at #[Token[gtEq: span = _], ...tokens = _] => return Generic.Cmp.ge
					else => break
				}
			}
			
			match This[parseType: tokens, true] {
				at Result[success: my right, tokens = _] => chain[add: #{span, op, right}]
				at my fail => return fail[Result[GenericCmpChain]]
			}
		}
		
		return Result[success: chain, tokens]
	}
	
	on [parseGenericRuleTerm: tokens (Tokens)] (Result[Generic.Rule]) {
		match tokens {
			at #[Token[lParen: my l], ...my rest] => return This[parseGenericRuleParen: l, rest]
			at #[Token[bang: my not], Token[lParen: my l], ...my rest] => match This[parseGenericRuleParen: l, rest] {
				at Result[success: my cond, my rest'] => Generic.Rule[:not :cond]
				at my fail => return fail
			}
			at #[Token[bang: my not], ...my rest] => match This[parseType: rest] {
				at Result[success: my type, my rest'] => return Result[success: Generic.Rule[:not :type], rest']
				at my fail => return fail[Result[Generic.Rule]]
			}
			at #[Token[typeName: _] || Token[wildcard], ..._] => match This[parseType: tokens, true] {
				at Result[success: my left, my rest] => match rest {
					at #[Token[question: my exists], ...my rest'] => return Result[success: Generic.Rule[type: left :exists], rest']
					
					at #[my op = Token[questionEq], ..._] => match This[parseGenericOpChain: rest, op] {
						at Result[success: my chain, my rest'] => return Result[success: Generic.Rule[:left eq: chain], rest']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[my op = Token[bangEq], ..._] => match This[parseGenericOpChain: rest, op] {
						at Result[success: my chain, my rest'] => return Result[success: Generic.Rule[:left ne: chain], rest']
						at my fail => return fail[Result[Generic.Rule]]
					}
					at #[my op = Token[of], ..._] => match This[parseGenericOpChain: rest, op] {
						at Result[success: my chain, my rest'] => return Result[success: Generic.Rule[:left of: chain], rest']
						at my fail => return fail[Result[Generic.Rule]]
					}
					
					at #[Token[lt] || Token[ltEq] || Token[gt] || Token[gtEq], ..._] => match This[parseGenericCmpChain: rest] {
						at Result[success: my chain, my rest'] => return Result[success: Generic.Rule[:left cmp: chain], rest']
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
			at Result[success: my rule, #[Token[rParen: my end], ...my rest']] => return Result[success: Generic.Rule[:begin paren: rule :end], rest']
			at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
			at my fail => return fail[fatalIfBad: tokens]
		}
	}
	
	
	on [parseUsePragma: generics (Array[Generic.Param]), span (Span), span' (Span), sym (Str), tokens (Tokens)] (Result[Decl]) {
		return Result[success: Use[:generics :span pragma: Ident[span: span' name: sym]], tokens]
	}
	
	;[on [parseUseDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseTypeSpec: tokens] {
			at Result[success: my spec, #[Token[label: "from" span: my span'], ...my rest]] => match This[parseType: rest] {
				at Result[success: my type, my rest'] => return Result[success: Use[:generics :span import: spec from: span', type], rest']
				at my fail => return fail[Result[Decl]]
			}
			at Result[success: my spec, my rest] => return Result[success: Use[:generics :span import: spec], rest]
			at my fail => return fail[Result[Decl] fatalIfBad: tokens]
		}
	}]
	on [parseUseDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
		match This[parseUseTree: tokens] {
			at Result[success: my spec, my rest] {
				my from, match rest at #[Token[span: my span' label: "from"], ...my rest'] {
					match rest' at #[Token[span: my span'' str: my segs], ...my rest''] {
						match This[parseStrSegs: segs] {
							at Result[success: #[Expr.StrPart[str: my path]], rest = _] => from = Maybe[the: Use.From[span: span' file: span'', path]]
							at Result[success: _, _] => return Result[fatal: tokens, Maybe[the: rest'']] ;@@ TODO: custom error message
							at my fail => return fail[Result[Decl] fatalIfBad: tokens]
						}
					} else {
						match This[parseType: rest'] {
							at Result[success: my type, rest = _] => from = Maybe[the: Use.From[span: span' :type]]
							at my fail => return fail[Result[Decl] fatalIfBad: tokens]
						}
					}
				} else {
					from = Maybe[none]
				}
				
				my as, match rest at #[Token[span: my span' label: "as"], ...my rest'] {
					match This[parseUseTree: rest'] {
						at Result[success: my tree, rest = _] => as = Maybe[the: #{span', tree}]
						at my fail => return fail[Result[Decl] fatalIfBad: tokens]
					}
				} else {
					as = Maybe[none]
				}
				
				return Result[success: Use[:generics :span import: spec :from :as], rest]
			}
			at my fail => return fail[Result[Decl]]
		}
	}
	
	on [parseUseTree: tokens (Tokens)] (Result[Use.Tree]) {
		match tokens {
			at #[Token[hashLBracket], ...my rest] {
				my types = #[]
				
				while true {
					match This[parseType: rest] {
						at Result[success: my type, my rest'] {
							types[add: type]
							
							match rest' {
								at #[Token[rBracket], ...my rest''] {
									return Result[success: Use.Tree[:types], rest'']
								}
								at #[] || #[_[isAnySep]] => return Result[eof: tokens]
								at #[_[isAnySep], ...rest = _] {}
								else => return Result[fatal: tokens, Maybe[the: rest']]
							}
						}
						at my fail => return fail[Result[Use.Tree]]
					}
				}
			}
			
			at #[Token[hashLParen], ...my rest] {
				my map = #[]
				
				while true {
					match This[parseType: rest] {
						at Result[success: my type, #[Token[eqGt: my span], ...my rest']] => match This[parseUseTree: rest'] {
							at Result[success: my tree, my rest''] {
								map[add: #{type, span, tree}]
								
								match rest'' {
									at #[Token[rParen], ...my rest'''] {
										return Result[success: Use.Tree[:map], rest''']
									}
									at #[] || #[_[isAnySep]] => return Result[eof: tokens]
									at #[_[isAnySep], ...rest = _] {}
									else => return Result[fatal: tokens, Maybe[the: rest'']]
								}
							}
							at my fail => return fail[Result[Use.Tree]]
						}
						at Result[success: _, my rest''] => return Result[fatal: tokens, Maybe[the: rest'']]
						at my fail => return fail[Result[Use.Tree]]
					}
				}
			}
			
			else => match This[parseType: tokens] {
				at Result[success: my type, my rest] => return Result[success: Use.Tree[:type], rest]
				at my fail => return fail[Result[Use.Tree]]
			}
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
								match This[parseAliasDeclAttrs: rest] {
									at Result[success: my attrs', rest = _] => return attrs'
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
					at #[Token[is: my i], Token[noinherit: my a], ...my rest] {
						return Result[success: Alias.Attrs[isNoinherit: i | a], rest]
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
					at Result[success: my type', rest = _] => type = Maybe[the: type']
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
						
						at #[Token[is: my i], Token[getter: my a], Token[litsym: my sym span: my span''], ...rest = _] {
							attrs |= Member.Attrs[is: i | a getter: Maybe[the: Ident[span: span'' name: sym]]]
						}
						at #[Token[is: my i], Token[getter: my a], ...rest = _] {
							attrs |= Member.Attrs[is: i | a getter: Maybe[none]]
						}
						
						at #[Token[is: my i], Token[setter: my a], Token[litsym: my sym span: my span''], ...rest = _] {
							attrs |= Member.Attrs[is: i | a setter: Maybe[the: Ident[span: span'' name: sym]]]
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
			at #[Token[eqGt], Token[lBracket], ...my rest] => match This[Type finishMsg: rest] {
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
					
					case' = Case[tag: Delims[:begin of: Case.Tag[multi: params] :end] :assoc]
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
			
			my body, match This[parseBody: rest] {
				at Result[success: my body', rest = _] => body = Maybe[the: body']
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
						
						at #[Token[is: my i], Token[macro: my a], ...rest = _] {
							attrs |= Init.Attrs[isMacro: i | a]
						}
						
						else => break
					}
				}
				
				my body, match This[parseBody: rest] {
					at Result[success: my body', rest = _] => body = Maybe[the: body']
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
			at #[Token[is: my i], Token[static: my a], ...my rest] => match This[parseBody: rest] {
				at Result[success: my body, my rest'] {
					return Result[success: DefaultInit[
						:span
						attrs: DefaultInit.Attrs[isStatic: i | a]
						:body
					], rest']
				}
				at my fail => return fail[Result[Decl] fatalIfBad: rest]
			}
			else => match This[parseBody: tokens] {
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
	
	on [parseOperatorDecl: generics (Array[Generic.Param]), span (Span), tokens (Tokens)] (Result[Decl]) {
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
					
					at #[Token[is: my i], Token[native: my a], Token[litsym: my sym' span: my span''], ...rest = _] {
						attrs |= Operator.Attrs[is: i | a native: Maybe[the: Ident[span: span'' name: sym']]]
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
			
			my body, match This[parseBody: rest] {
				at Result[success: my body', rest = _] => body = Maybe[the: body']
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
			match This[parseBody: rest] {
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
			match This[parseBody: tokens] {
				at Result[success: my body, my rest] {
					return Result[success: Deinit[:span attrs: Deinit.Attrs[empty] :body], rest]
				}
				at my fail => return fail[Result[Decl] fatalIfBad: tokens]
			}
		}
	}
	
	
	;== Types
	
	on [parseTypeParents: tokens (Tokens), allowEOL (Bool) = true] (Result[Array[Type]]) {
		match tokens at #[Token[of], ...my rest] {
			match This[parseType: rest] {
				at Result[success: my type, rest = _] {
					my parents = #[type]
					
					while true {
						match rest at #[Token[comma], ...my rest'] {
							match This[parseType: rest'] {
								at Result[success: my type', rest = _] => parents[add: type']
								at _ if allowEOL => break
								at my fail => return fail[Result[Array[Type]] fatalIfBad: rest]
							}
						} else {
							break
						}
					}
					
					return Result[success: parents, rest]
				}
				at my fail => return fail[Result[Array[Type]] fatalIfBad: rest]
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
		match tokens at #[Token[hashLBracket: my begin], ...my rest] {
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
			leading = Array[Span] #[]
		}
		
		match This[parseTypeSeg: rest] {
			at Result[success: my seg, my rest'] {
				match rest' at #[Token[dot], Token[typeName: _], ..._] {
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
				at Result[failure: _, _] => return Result[success: Type.Seg[name: name'], rest]
				at my fail => return fail[Result[Type.Seg]]
			}
		} else {
			return Result[failure: tokens, Maybe[none]]
		}
	}
	
	on [parseTypeSegs: tokens (Tokens)] (Result[Array[Type.Seg]]) {
		match tokens at #[Token[dot], ...my rest] {
			match This[parseTypeSeg: rest] {
				at Result[success: my seg, my rest'] => match This[parseTypeSegs: rest'] {
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
	
	on [parseTypeArgs: tokens (Tokens)] (Result[Delims[Array[Type]]]) {
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
	
	;@@ TODO: enforce short arrow stmt
	on [parseBody: tokens (Tokens)] (Result[Stmt.Body]) {
		match tokens at #[Token[eqGt: my arrow], ...my rest] {
			match This[parseStmt: rest] {
				at Result[success: my stmt, my rest'] => return Result[success: Stmt.Body[:arrow :stmt], rest']
				at my fail => return fail[Result[Stmt.Body] fatalIfFailed]
			}
		} else {
			match This[parseBlock: tokens] {
				at Result[success: my block, my rest] => return Result[success: Stmt.Body[:block], rest]
				at my fail => return fail[Result[Stmt.Body]]
			}
		}
	}
	
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
			at #[Token[if: my span], ...my rest] => match This[parseExpr: rest] {
				at Result[success: my cond, my rest'] => match This[parseThenStmt: rest'] {
					at Result[success: my then = Then[block: _], #[Token[else: my span'], ...my rest'']] {
						match This[parseBlock: rest''] {
							at Result[success: my block, my rest'''] {
								return Result[
									success: Stmt[if: span, cond :then else: Maybe[the: #{span', block}]],
									rest'''
								]
							}
							at my fail => return fail[Result[Stmt]]
						}
					}
					at Result[success: my then, my rest''] {
						return Result[success: Stmt[if: span, cond :then else: Maybe[none]], rest'']
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
							at my fail => return fail[Result[Stmt] fatalIfFailed]
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
								at Result[success: my case', my rest'''] {
									cases[add: case']
									
									match rest''' {
										at #[Token[rBrace], ...my rest''''] {
											return Result[
												success: Stmt[:match :value :cases else: Maybe[none]],
												rest''''
											]
										}
										at #[] || #[_[isAnySep]] => return Result[eof: tokens]
										at #[_[isAnySep], rest' = _] {}
										else => return Result[fatal: tokens, Maybe[the: rest''']]
									}
								}
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[else: my span], ...my rest'''] => match This[parseThenStmt: rest'''] {
								at Result[success: my else', #[Token[rBrace], ...my rest'''']] {
									return Result[
										success: Stmt[:match :value :cases else: Maybe[the: #{span, else'}]],
										rest''''
									]
								}
								at Result[success: _, my rest''''] => return Result[fatal: tokens, Maybe[the: rest'''']]
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
							at #[Token[rBrace], ...my rest'''] {
								return Result[
									success: Stmt[:match :value :cases else: Maybe[none]],
									rest'''
								]
							}
							else => return Result[fatal: tokens, Maybe[the: rest']]
						}
					}
				}
				
				at Result[success: my value, #[Token[at: my at], ...my rest']] => match This[parseExpr: rest'] {
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

						match This[parseThenStmt: rest''] {
							at Result[success: my then = Then[block: _], #[Token[else: my span], ...my rest''']] {
								match This[parseBlock: rest'''] {
									at Result[success: my else', my rest''''] {
										return Result[
											success: Stmt[:match :value :at, pattern if: cond :then else: Maybe[the: #{span, else'}]],
											rest''''
										]
									}
									at my fail => return fail[Result[Stmt] fatalIfFailed]
								}
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
			
			at #[Token[while: my while], ...my rest] {
				#{my label, rest} = This[parseLoopLabel: rest]
				
				match This[parseExpr: rest] {
					at Result[success: my cond, my rest'] => match This[parseThenStmt: rest'] {
						at Result[success: my do, my rest''] => return Result[success: Stmt[:while, cond :label :do], rest'']
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
					at my fail => return fail[Result[Stmt] fatalIfFailed]
				}
			}
			
			at #[Token[do: my do], ...my rest] {
				#{my label, rest} = This[parseLoopLabel: rest]
				
				match This[parseBlock: rest] {
					at Result[success: my block, #[Token[while: my while], ...my rest']] => match This[parseExpr: rest'] {
						at Result[success: my cond, my rest''] => return Result[success: Stmt[:do :label, block :while, cond], rest'']
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
					at Result[success: my block, my rest'] => return Result[success: Stmt[:do :label, block], rest']
					at my fail => return fail[Result[Stmt] fatalIfFailed]
				}
			}
			
			at #[Token[for: my span], ...my rest] => match This[parseExpr: rest] {
				at Result[success: my var, my rest'] => match rest' {
					at #[Token[label: "from" span: my span'], ...my rest''] {
						return This[parseLoopRange: span, var, Stmt.Loop.Start[from] -> span = span', rest'']
					}
					at #[Token[label: "after" span: my span'], ...my rest''] {
						return This[parseLoopRange: span, var, Stmt.Loop.Start[after] -> span = span', rest'']
					}
					at #[Token[comma], ...my rest''] => match This[parseExpr: rest''] {
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
					return Result[success: Stmt[:break depth: span, {
						match int at "0" {}
						match span at _ {}
						match exp at Maybe[the: my exp'] {
							return "\(int)e\(exp')"[Int]
						} else {
							return int[Int]
						}
					}], rest']
				}
				at #[Token[span: my span litsym: my label], ...my rest'] {
					return Result[success: Stmt[:break label: span, label], rest']
				}
				else => return Result[success: Stmt[:break], rest]
			}
			
			at #[Token[next: my next], ...my rest] => match rest {
				at #[Token[int: my int exp: my exp span: my span], ...my rest'] {
					return Result[success: Stmt[:next depth: span, {
						match exp at Maybe[the: my exp'] {
							return "\(int)e\(exp')"[Int]
						} else {
							return int[Int]
						}
					}], rest']
				}
				at #[Token[span: my span litsym: my label], ...my rest'] {
					return Result[success: Stmt[:next label: span, label], rest']
				}
				else => return Result[success: Stmt[:next], rest]
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
								at Result[success: my case', my rest'''] {
									cases[add: case']
									
									match rest''' {
										at #[Token[rBrace], ...my rest''''] {
											return Result[
												success: Stmt[:try, block :catch :cases else: Maybe[none]],
												rest''''
											]
										}
										at #[] || #[_[isAnySep]] => return Result[eof: tokens]
										at #[_[isAnySep], rest' = _] {}
										else => return Result[fatal: tokens, Maybe[the: rest''']]
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
	

	on [parseThenStmt: tokens (Tokens)] (Result[Then]) {
		match tokens at #[Token[eqGt: my span], ...my rest] {
			match This[parseStmt: rest] {
				at Result[success: my stmt, my rest'] => return Result[success: Then[stmt: span, stmt], rest']
				at my fail => return fail[Result[Then] fatalIfFailed]
			}
		} else {
			match This[parseBlock: tokens] {
				at Result[success: my block, my rest] => return Result[success: Then[:block], rest]
				at my fail => return fail[Result[Then] fatalIfBad: tokens]
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
	
	on [parsePatternAtStmt: span (Span), tokens (Tokens)] (Result[Stmt.PatternAt]) {
		match This[parseExpr: tokens] {
			at Result[success: my pattern, my rest] {
				my cond, match rest at #[Token[lSep]?, Token[if: my span'], ...my rest'] {
					match This[parseExpr: rest'] {
						at Result[success: my cond', rest = _] => cond = Maybe[the: cond']
						at my fail => return fail[Result[Stmt.PatternAt] fatalIfFailed]
					}
				} else {
					cond = Maybe[none]
				}
				
				match This[parseThenStmt: rest] {
					at Result[success: my then, my rest'] => return Result[success: Stmt.PatternAt[:span :pattern :cond :then], rest]
					at my fail => return fail[Result[Stmt.PatternAt] fatalIfFailed]
				}
			}
			at my fail => return fail[Result[Stmt.PatternAt] fatalIfFailed]
		}
	}
	
	
	on [parseLoopLabel: tokens (Tokens)] (Tuple[Maybe[Tuple[Span, Ident]], Tokens]) is inline {
		match tokens at #[Token[span: my span label: "label"], Token[span: my span' litsym: my name], ...my rest] {
			return #{Maybe[the: Ident[span: span' :name]], rest}
		} else {
			return #{Maybe[none], tokens}
		}
	}
	
	on [parseLoopIn: span (Span), var (Expr), var' (Maybe[Expr]), tokens (Tokens)] (Result[Stmt]) {
		match tokens at #[Token[span: my inSpan label: "in:"], ...my rest] {
			match This[parseExpr: rest] {
				at Result[success: my inExpr, my rest'] {
					my cond, match rest' at #[Token[span: my span' label: "while"], ...my rest''] {
						match This[parseExpr: rest''] {
							at Result[success: my cond', rest' = _] => cond = Maybe[the: #{span', cond'}]
							at my fail => return fail[Result[Stmt] fatalIfFailed]
						}
					} else {
						cond = Maybe[none]
					}
					
					#{my label, rest'} = This[parseLoopLabel: rest']
					
					match This[parseThenStmt: rest'] {
						at Result[success: my do, my rest''] {
							return Result[
								success: Stmt[for: span :var :var' in: #{inSpan, inExpr} while: cond :label :do],
								rest''
							]
						}
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
				}
				at my fail => return fail[Result[Stmt] fatalIfFailed]
			}
		} else {
			return Result[fatal: tokens, Maybe[none]]
		}
	}
	
	on [parseLoopRange: span (Span), var (Expr), start (Stmt.Loop.Start), tokens (Tokens)] (Result[Stmt]) {
		match This[parseExpr: tokens] {
			at Result[success: start.expr = _, my rest] => match rest {
				at #[Token[span: my span' label: my stopLabel], ...my rest'] {
					my stop, match stopLabel {
						at "to" => stop = Stmt.Loop.Stop[to]
						at "upto" => stop = Stmt.Loop.Stop[upto]
						at "downto" => stop = Stmt.Loop.Stop[downto]
						at "times" => stop = Stmt.Loop.Stop[times]
						else => return Result[fatal: tokens, Maybe[the: rest']]
					}
					
					stop.span = span'
					
					match This[parseExpr: rest'] {
						at Result[success: stop.expr = _, rest' = _] {
							my step, match rest at #[Token[span: my span'' label: "by"], ...my rest''] {
								match This[parseExpr: rest''] {
									at Result[success: my step', rest' = _] => step = Maybe[the: #{span'', step'}]
									at my fail => return fail[Result[Stmt] fatalIfFailed]
								}
							} else {
								step = Maybe[none]
							}
							
							my cond, match rest' at #[Token[span: my span'' label: "while"], ...my rest''] {
								match This[parseExpr: rest''] {
									at Result[success: my cond', rest' = _] => cond = Maybe[the: #{span'', cond'}]
									at my fail => return fail[Result[Stmt] fatalIfFailed]
								}
							} else {
								cond = Maybe[none]
							}
							
							#{my label, rest'} = This[parseLoopLabel: rest']
							
							match This[parseThenStmt: rest'] {
								at Result[success: my do, my rest''] {
									return Result[
										success: Stmt[for: span :var :start :stop :step while: cond :label :do],
										rest''
									]
								}
								at my fail => return fail[Result[Stmt] fatalIfFailed]
							}
						}
						at my fail => return fail[Result[Stmt] fatalIfFailed]
					}
				}
				else => return Result[fatal: tokens, Maybe[the: rest]]
			}
			at my fail => return fail[Result[Stmt] fatalIfFailed]
		}
	}


	;== Expressions

	on [parseBasicExpr: tokens (Tokens)] (Result[Expr]) {
		match tokens {
			at #[Token[span: my span, name: my name] = _[asSoftName], ...my rest] {
				return Result[success: Expr[name: Ident[:span :name]], rest]
			}
			
			at #[Token[span: my span litsym: my name], ...my rest] => return Result[success: Expr[litsym: Ident[:span :name]], rest]

			at #[Token[span: my span int: my int exp: Maybe[none]], ...my rest] {
				return Result[success: Expr[:span int: int[Int]], rest]
			}
			at #[Token[span: my span int: my int exp: Maybe[the: my exp]], ...my rest] {
				return Result[success: Expr[:span int: int[Int] exp: exp[Int]], rest]
			}
			;@@TODO: hex

			at #[Token[span: my span int: my int dec: my dec exp: Maybe[none]], ...my rest] {
				return Result[success: Expr[:span int: int[Int] :dec], rest]
			}
			at #[Token[span: my span int: my int dec: my dec exp: Maybe[the: my exp]], ...my rest] {
				return Result[success: Expr[:span int: int[Int] :dec exp: exp[Int]], rest]
			}

			at #[Token[span: my span str: my segs], ...my rest] => match This[parseStrSegs: segs] {
				at Result[success: my parts, _] => return Result[success: Expr[:span str: parts], rest]
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[span: my span char: my char], ...my rest] => return Result[success: Expr[:span :char], rest]

			at #[Token[span: my span bool: my bool], ...my rest] => return Result[success: Expr[:span :bool], rest]

			at #[Token[this: my this], ...my rest] => return Result[success: Expr[:this], rest]

			at #[Token[span: my span anonArg: my anonArg depth: my depth], ...my rest] {
				return Result[success: Expr[:span :anonArg :depth], rest]
			}

			at #[my head = Token[typeName: _] || Token[wildcard: _], ...my rest] => match This[parseType: tokens] {
				at Result[success: my type, my rest'] => return Result[success: Expr[:type], rest']
				at my fail = Result[failure: _, _] => match head {
					at Token[wildcard: my wildcard] => return Result[success: Expr[:wildcard], rest]
					else => return fail[Result[Expr]]
				}
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[hashLBracket: my begin], Token[rBracket: my end], ...my rest] {
				return Result[success: Expr[:begin array: #[] :end], rest]
			}
			at #[Token[hashLBracket: my begin], ...my rest] => match This[parseArrayContents: rest] {
				at Result[success: #{my array, my end}, my rest'] => return Result[success: Expr[:begin :array :end], rest']
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[hashLParen: my begin], Token[rParen: my end], ...my rest] {
				return Result[success: Expr[:begin hash: #[] :end], rest]
			}
			at #[Token[hashLParen: my begin], ...my rest] => match This[parseHashContents: rest] {
				at Result[success: #{my hash, my end}, my rest'] => return Result[success: Expr[:begin :hash :end], rest']
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[hashLBrace: my begin], Token[rBrace: my end], ...my rest] {
				return Result[success: Expr[:begin tuple: #[] :end], rest]
			}
			at #[Token[hashLBrace: my begin], ...my rest] => match This[parseTupleContents: rest] {
				at Result[success: #{my tuple, my end}, my rest'] => return Result[success: Expr[:begin :tuple :end], rest']
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[lParen: my begin], ...my rest] => match This[parseParenContents: rest] {
				at Result[success: #{my paren, my end}, my rest'] => return Result[success: Expr[:begin :paren :end], rest']
				at my fail => return fail[Result[Expr]]
			}

			at #[Token[lBracket: my begin], ...my rest] => match This[parseFullExpr: rest] {
				at Result[success: Expr[type: my type], #[Token[lSep]?, ...my rest']] => match This[Type finishMsg: rest'] {
					at Result[success: #{my message, my end}, my rest''] => return Result[success: Expr[:type :begin :message :end], rest'']
					at my fail => return fail[Result[Expr] fatalIfFailed]
				}

				at Result[success: my expr, #[Token[lSep]?, ...my rest']] => match This[Expr finishMsg: rest'] {
					at Result[success: #{my message, my end}, my rest''] => return Result[success: Expr[:expr :begin :message :end], rest'']
					at my fail => return fail[Result[Expr] fatalIfFailed]
				}

				at my fail => return fail[Result[Expr] fatalIfFailed]
			}

			at #[Token[lBrace: my begin], Token[barBar], ...my rest] => return This[finishFunc: begin, #[], rest]
			at #[Token[lBrace: my begin], Token[bar], Token[bar], ...my rest] => return This[finishFunc: begin, #[], rest]
			at #[Token[lBrace: my begin], Token[bar], ...my rest] {
				my params = #[]

				while true {
					match rest at #[Token[span: my span, name: my name] = _[asSoftName], ...my rest'] {
						match rest' at #[Token[lParen], ..._] {
							match This[parseTypeAnno: rest'] {
								at Result[success: my type, rest = _] => params[add: #{Ident[:span :name], Maybe[the: type]}]
								at my fail => return fail[Result[Expr] fatalIfFailed]
							}
						} else {
							rest = rest'
							params[add: #{Ident[:span :name], Maybe[none]}]
						}
					} else {
						return Result[fatal: tokens, Maybe[the: rest]]
					}

					match rest {
						at #[Token[bar], ...rest = _] => break
						at #[Token[comma], ...rest = _] => next
						else => return Result[fatal: tokens, Maybe[the: rest]]
					}
				}

				return This[finishFunc: begin, params, rest]
			}
			at #[Token[lBrace], ..._] => match This[parseBlock: tokens] {
				at Result[success: my block, my rest] => return Result[success: Expr[:block], rest]
				at my fail => return fail[Result[Expr] fatalIfFailed]
			}

			at #[Token[my: my my], ...my rest] => match rest {
				at #[Token[span: my span, name: my name] = _[asSoftName], ...my rest'] {
					my type, match This[parseTypeAnno: rest'] {
						at Result[success: my type', rest' = _] => type = Maybe[the: type']
						at Result[failure: _, _] => type = Maybe[none]
						at my fail => return fail[Result[Expr]]
					}

					my value, match rest' at #[Token[eq], ...my rest''] {
						match This[parseFullExpr: rest''] {
							at Result[success: my expr, rest' = _] => value = Maybe[the: expr]
							at my fail => return fail[Result[Expr] fatalIfFailed]
						}
					} else {
						value = Maybe[none]
					}

					return Result[success: Expr[:my name: Ident[:span :name] :type :value], rest']
				}

				else => return Result[fatal: tokens, Maybe[none]]
			}

			else => return Result[failure: tokens, Maybe[none]]
		}
	}

	on [finishFunc: begin (Span), params (Array[Tuple[Ident, Maybe[Type]]]), tokens (Tokens)] (Result[Expr]) {
		match This[parseTypeAnno: tokens] {
			at Result[success: my ret, #[Token[rBrace: my end], ...my rest]] {
				return Result[success: Expr[:begin :params return: Maybe[the: ret] func: #[] :end], rest]
			}
			at Result[success: my ret, #[Token[lSep]?, ...my rest]] => return This[finishFuncBody: begin, params, Maybe[the: ret], rest]
			at Result[failure: _, _] => return This[finishFuncBody: begin, params, Maybe[none], {
				match tokens at #[Token[lSep], ...my rest] {
					return rest
				} else {
					return tokens
				}
			}]
			at my fail => return fail[Result[Expr]]
		}
	}

	on [finishFuncBody: begin (Span), params (Array[Tuple[Ident, Maybe[Type]]]), ret (Maybe[Type]), tokens (Tokens)] (Result[Expr]) {
		my stmts = #[]
		my rest = tokens

		while true {
			match This[parseStmt: rest] {
				at Result[success: my stmt, my rest'] {
					stmts[add: stmt]

					match rest' {
						at #[Token[rBrace: my end], ...my rest''] => return Result[success: Expr[:begin :params return: ret func: stmts :end], rest'']
						at #[] || #[_[isAnySep]] => return Result[eof: tokens]
						at #[_[isAnySep], ...rest = _] {}
						else => return Result[fatal: tokens, Maybe[the: rest']]
					}
				}
				at my fail => return fail[Result[Expr] fatalIfBad: rest]
			}
		}
	}

	on [parseArrayContents: tokens (Tokens)] (Result[Tuple[Array[Expr], Span]]) {
		my exprs = #[]
		my rest = tokens

		while true {
			match This[parseFullExpr: rest] {
				at Result[success: my expr, my rest'] {
					exprs[add: expr]

					match rest' {
						at #[Token[rBracket: my end], ...my rest''] => return Result[success: #{exprs, end}, rest'']
						at #[] || #[_[isAnySep]] => return Result[eof: tokens]
						at #[_[isAnySep], ...rest = _] {}
						else => return Result[fatal: tokens, Maybe[the: rest']]
					}
				}
				at my fail => return fail[Result[Tuple[Array[Expr], Span]] fatalIfBad: rest]
			}
		}
	}

	on [parseTupleContents: tokens (Tokens)] (Result[Tuple[Array[Expr], Span]]) {
		my exprs = #[]
		my rest = tokens

		while true {
			match This[parseFullExpr: rest] {
				at Result[success: my expr, my rest'] {
					exprs[add: expr]

					match rest' {
						at #[Token[rBrace: my end], ...my rest''] => return Result[success: #{exprs, end}, rest'']
						at #[] || #[_[isAnySep]] => return Result[eof: tokens]
						at #[_[isAnySep], ...rest = _] {}
						else => return Result[fatal: tokens, Maybe[the: rest']]
					}
				}
				at my fail => return fail[Result[Tuple[Array[Expr], Span]] fatalIfBad: rest]
			}
		}
	}
	
	alias HashContents is hidden = Tuple[Array[Tuple[Expr, Expr]], Span]
	on [parseHashContents: tokens (Tokens)] (Result[HashContents]) {
		my pairs = #[]
		my rest = tokens

		while true {
			match This[parseExpr: rest] {
				at Result[success: my key, #[Token[eqGt], ...my rest']] => match This[parseFullExpr: rest'] {
					at Result[success: my value, my rest''] {
						pairs[add: #{key, value}]

						match rest'' {
							at #[Token[rParen: my end], ...my rest'''] => return Result[success: #{pairs, end}, rest''']
							at #[] || #[_[isAnySep]] => return Result[eof: tokens]
							at #[_[isAnySep], ...rest = _] {}
							else => return Result[fatal: tokens, Maybe[the: rest'']]
						}
					}
					at my fail => return fail[Result[HashContents]]
				}
				at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
				at my fail => return fail[Result[HashContents] fatalIfBad: rest]
			}
		}
	}
	
	on [parseStrSegs: segs (Array[StrSegment])] (Result[Array[Expr.StrPart]]) {
		my parts = #[]
		my buf = ""

		for my seg in: segs {
			match seg {
				at StrSegment[str: my str] => buf[add: str]
				at StrSegment[char: my char] => buf[add: char]
				at StrSegment[code: my code] => match This[parseFullExpr: code] {
					at Result[success: my expr, #[]] {
						if buf? {
							parts[add: Expr.StrPart[str: buf]]
							buf = ""
						}

						parts[add: Expr.StrPart[code: expr]]
					}
					at Result[success: _, my rest] => return Result[fatal: code, Maybe[the: rest]]
					at my fail => return fail[Result[Array[Expr.StrPart]]]
				}
			}
		}

		if buf? {
			parts[add: Expr.StrPart[str: buf]]
		}

		return Result[success: parts, Tokens #[]] ;@@ TODO: generalize Result so we don't need a dummy series
	}


	on [skipParen: tokens (Tokens)] (Tokens) {
		while true {
			match tokens {
				at #[Token[lParen] || Token[hashLParen], ...my rest] => tokens = This[skipParen: rest][next]
				at #[Token[lBracket] || Token[hashLBracket], ...my rest] => tokens = This[skipBracket: rest][next]
				at #[Token[lBrace] || Token[hashLBrace], ...my rest] => tokens = This[skipBrace: rest][next]
				at #[Token[lSep], Token[cascade: _], ...my rest] => tokens = rest
				at #[Token[lSep], ..._] => tokens[removeAt: 0]
				at #[Token[rParen], ..._] => return tokens
				at #[_, ...tokens = _] {}
				at #[] => throw "Error!"
			}
		}
	}

	on [skipBracket: tokens (Tokens)] (Tokens) {
		while true {
			match tokens {
				at #[Token[lParen] || Token[hashLParen], ...my rest] => tokens = This[skipParen: rest][next]
				at #[Token[lBracket] || Token[hashLBracket], ...my rest] => tokens = This[skipBracket: rest][next]
				at #[Token[lBrace] || Token[hashLBrace], ...my rest] => tokens = This[skipBrace: rest][next]
				at #[Token[rBracket], ..._] => return tokens
				at #[_, ...tokens = _] {}
				at #[] => throw "Error!"
			}
		}
	}

	on [skipBrace: tokens (Tokens)] (Tokens) {
		while true {
			match tokens {
				at #[Token[lParen] || Token[hashLParen], ...my rest] => tokens = This[skipParen: rest][next]
				at #[Token[lBracket] || Token[hashLBracket], ...my rest] => tokens = This[skipBracket: rest][next]
				at #[Token[lBrace] || Token[hashLBrace], ...my rest] => tokens = This[skipBrace: rest][next]
				at #[Token[rBrace], ..._] => return tokens
				at #[_, ...tokens = _] {}
				at #[] => throw "Error!"
			}
		}
	}

	on [removeNewlines: tokens (Tokens)] (Bool) {
		while true {
			match tokens {
				at #[Token[lParen] || Token[hashLParen], ...my rest] => tokens = This[skipParen: rest]
				at #[Token[lBracket] || Token[hashLBracket], ...my rest] => tokens = This[skipBracket: rest]
				at #[Token[lBrace] || Token[hashLBrace], ...my rest] => tokens = This[skipBrace: rest]
				at #[Token[rParen], ..._] => return true
				at #[Token[lSep], Token[cascade: _], ...tokens = _] {}
				at #[Token[lSep], ..._] {
					tokens[removeAt: 0]
					tokens++
				}
				at #[_, ...tokens = _] {}
				at #[] => return false
			}
		}
	}

	on [parseParenContents: tokens (Tokens)] (Result[Tuple[Array[Expr], Span]]) {
		my exprs = #[]

		if !This[removeNewlines: tokens] => return Result[eof: tokens]

		my rest = tokens
		my leadingOp = {
			match rest {
				at #[Token[andAnd], ...rest = _] => return Maybe[the: Infix[and]]
				at #[Token[barBar], ...rest = _] => return Maybe[the: Infix[or]]
				at #[Token[caretCaret], ...rest = _] => return Maybe[the: Infix[xor]]
				at #[Token[bangBang], ...rest = _] => return Maybe[the: Infix[nor]]
				else => return Maybe[none]
			}
		}

		while true {
			match This[parseFullExpr: rest] {
				at Result[success: my expr, my rest'] {
					exprs[add: expr]

					match rest' {
						at #[Token[rParen: my end], ...my rest''] {
							match leadingOp at Maybe[the: my op] {
								match exprs[at: 0] at !Expr[left: _ infix: _, op right: _] {
									return Result[fatal: tokens, Maybe[none]]
								}
							}

							return Result[success: #{exprs, end}, rest'']
						}
						at #[] || #[_[isAnyComma]] => return Result[eof: tokens]
						at #[_[isAnyComma], ...rest = _] {}
						else => return Result[fatal: tokens, Maybe[the: rest']]
					}
				}
				at my fail => return fail[Result[Tuple[Array[Expr], Span]]]
			}
		}
	}


	;== Message parsing

	alias MultiMsg is hidden = Tuple[Array[Label], Span]
	on [parseMultiMsgLabels: tokens (Tokens)] (Result[MultiMsg]) {
		my rest = tokens
		my first, match tokens {
			at #[Token[span: my span label: my name], Token[lSep]?, ...my rest'] {
				match This[parseFullExpr: rest'] {
					at Result[success: my expr, rest = _] {
						my label = Ident[:span :name]
						first = Label[:label :expr]
					}
					at my fail => return fail[Result[MultiMsg]]
				}
			}

			at #[Token[span: my span punned: my name], ...rest = _] {
				my label = Ident[:span :name]
				first = Label[:label]
			}

			else => return Result[fatal: tokens, Maybe[none]]
		}
		my labels = #[first]

		while true {
			match rest {
				at #[Token[rBracket: my end], ...my rest'] {
					return Result[success: #{labels, end}, rest']
				}

				at #[Token[lSep]?, Token[span: my span label: my name], ...my rest'] {
					match This[parseFullExpr: rest'] {
						at Result[success: my expr, rest = _] {
							my label = Ident[:span :name]
							labels[add: Label[:label :expr]]
						}
						at my fail => return fail[Result[MultiMsg]]
					}
				}

				at #[Token[lSep]?, Token[span: my span punned: my name], ...rest = _] {
					my label = Ident[:span :name]
					labels[add: Label[:label]]
				}

				at #[_[isAnyComma], ...my rest'] => match This[parseFullExpr: rest'] {
					at Result[success: my expr, rest = _] => labels[add: Label[:expr]]
					at my fail => return fail[Result[MultiMsg]]
				}

				else => return Result[fatal: tokens, Maybe[the: rest]]
			}
		}
	}

	alias ExprMsg is hidden = Tuple[Message[Expr], Span]
	category Expr {
		on [finishMsg: tokens (Tokens)] (Result[ExprMsg]) is static {
			match tokens {
				at #[Token[typeName: _] || Token[wildcard], ..._] => match This[parseType: tokens] {
					at Result[success: my type, #[Token[lSep]?, ...my rest]] => match rest {
						at #[Token[label: _] || Token[punned: _], ..._] => match This[parseMultiMsgLabels: rest] {
							at Result[success: #{my labels, my end}, my rest'] {
								return Result[success: #{Message[category: Maybe[the: type] multi: labels], end}, rest']
							}
							at my fail => return fail[Result[ExprMsg]]
						}
						
						at #[Token[span: my span name: my name] = _[asAnyName], ...my rest'] {
							match rest' at #[Token[rBracket: my end], ...my rest''] {
								return Result[success: #{Message[category: Maybe[the: type] single: Ident[:span :name]], end}, rest'']
							} else {
								return Result[fatal: tokens, Maybe[the: rest']]
							}
						}

						at #[Token[typeName: _], ..._] => match This[parseType: rest] {
							at Result[success: my cast, #[Token[rBracket: my end], ...my rest']] {
								return Result[success: #{Message[category: Maybe[the: type] :cast], end}, rest']
							}
							at Result[success: _, my rest'] => return Result[fatal: tokens, Maybe[the: rest']]
							at my fail => return fail[Result[ExprMsg]]
						}

						at #[Token[rBracket: my end], ...my rest'] {
							return Result[success: #{Message[cast: type], end}, rest']
						}

						else => return Result[fatal: tokens, Maybe[none]]
					}
					at my fail => return fail[Result[ExprMsg]]
				}

				at #[Token[label: _] || Token[punned: _], ..._] => match This[parseMultiMsgLabels: tokens] {
					at Result[success: #{my labels, my end}, my rest] {
						return Result[success: #{Message[multi: labels], end}, rest]
					}
					at my fail => return fail[Result[ExprMsg]]
				}

				at #[Token[span: my span name: my name] = _[asAnyName], ...my rest] {
					match rest at #[Token[rBracket: my end], ...my rest'] {
						return Result[success: #{Message[single: Ident[:span :name]], end}, rest']
					} else {
						return Result[fatal: tokens, Maybe[the: rest]]
					}
				}

				else => return Result[fatal: tokens, Maybe[none]]
			}
		}
	}

	alias TypeMsg is hidden = Tuple[Message[Type], Span]
	category Type {
		on [finishMsg: tokens (Tokens)] (Result[TypeMsg]) is static {
			match This[Expr finishMsg: tokens] {
				at Result[success: #{Message[cast: _], _}, _] => return Result[fatal: tokens, Maybe[none]]
				at my result => return result[Result[TypeMsg]]
			}
		}
	}


	;== Expression parsing

	;[ Highest ---> Lowest
	==|  1:  a.b, a[b], a++, a--, a?
	==|  2:  ++a, --a, -a, ~a, !a
	==|  3:  a -> [b], a -> [b] = c, a -> b = c, a -> {...}   (Single line only)
	==|  4:  #a b
	==|  5:  a ** b
	==|  6:  a * b, a / b, a // b, a % b
	==|  7:  a + b, a - b
	==|  8:  a & b, a | b, a ^ b, a >> b, a << b
	==|  9:  a %% b
	==| 10:  a ?= b, a != b, a > b, a >= b, a < b, a <= b
	==| 11:  a && b, a || b, a ^^ b, a !! b
	==| 12:  a = b, a += b, a -= b, a *= b, a **= b, a /= b, a //= b, a %= b, a %%= b, a &= b, a |= b, a ^= b, a >>= b, a <<= b, a &&= b, a ||= b, a ^^= b, a !!= b
	==| 13:  ...a
	]

	on [parseExpr: tokens (Tokens)] (Result[Expr]) => return This[parseExpr13: tokens]


	;[
	==| Type.member
	==| Type[message]
	==| Type <literal>
	==| expr.member
	==| expr[message]
	==| expr?
	==| expr++
	==| expr--
	]

	on [parseExpr1: tokens (Tokens)] (Result[Expr]) {
		my result = {
			match This[parseBasicExpr: tokens] {
				at my res = Result[success: Expr[type: my type], my rest] => match rest {
					at #[Token[dot], Token[span: my span name: my name], ...my rest'] {
						return Result[success: Expr[:type member: Ident[:span :name]], rest']
					}

					at #[Token[lBracket: my begin], ...my rest'] => match This[Type finishMsg: rest'] {
						at Result[success: #{my message, my end}, my rest''] {
							return Result[success: Expr[:type :begin :message :end], rest'']
						}
						at my fail => return fail[Result[Expr] fatalIfBad: tokens]
					}

					at #[Token[minus], Token[int: _ exp: _] <= _ <= Token[hex: _], ..._] => match This[parseExpr2: rest] {
						at Result[success: my literal, my rest'] => return Result[success: Expr[:type :literal], rest']
						at my fail => return fail[fatalIfBad: tokens]
					}

					at (
						|| #[
							(
							|| Token[int: _ exp: _] <= _ <= Token[bool: _]
							|| Token[hashLParen] <= _ <= Token[hashLBrace]
							)
							..._
						]
						|| #[
							Token[lBrace]
							Token[bar] || Token[barBar]
							..._
						]
					) => match This[parseBasicExpr: rest] {
						at Result[success: my literal, my rest'] => return Result[success: Expr[:type :literal], rest']
						at my fail => return fail[fatalIfBad: tokens]
					}

					else => return res
				}
				
				at my res => return res
			}
		}

		match result at Result[success: my expr, my rest] {
			while true {
				match rest {
					at #[Token[dot], Token[span: my span name: my name], ...rest = _] {
						expr = Expr[:expr member: Ident[:span :name]]
					}

					at #[Token[lBracket: my begin], ...my rest'] => match This[Expr finishMsg: rest'] {
						at Result[success: #{my message, my end}, rest = _] {
							expr = Expr[:expr :begin :message :end]
						}
						at my fail => return fail[Result[Expr] fatalIfBad: tokens]
					}

					at #[Token[question: my span], ...rest = _] {
						expr = Expr[left: expr suffix: span, Suffix.truthy]
					}

					at #[Token[plusPlus: my span], ...rest = _] {
						expr = Expr[left: expr suffix: span, Suffix.incr]
					}

					at #[Token[minusMinus: my span], ...rest = _] {
						expr = Expr[left: expr suffix: span, Suffix.decr]
					}

					else => break
				}
			}

			return Result[success: expr, rest]
		} else {
			return result
		}
	}


	;[
	==| -expr
	==| ++expr
	==| --expr
	==| ~expr
	==| !expr
	]

	on [parseExpr2: tokens (Tokens)] (Result[Expr]) {
		match tokens {
			at #[Token[minus: my span], ...my rest] => return This[parseExpr2Rest: rest, span, Prefix.neg]
			at #[Token[plusPlus: my span], ...my rest] => return This[parseExpr2Rest: rest, span, Prefix.incr]
			at #[Token[minusMinus: my span], ...my rest] => return This[parseExpr2Rest: rest, span, Prefix.decr]
			at #[Token[tilde: my span], ...my rest] => return This[parseExpr2Rest: rest, span, Prefix.compl]
			at #[Token[bang: my span], ...my rest] => return This[parseExpr2Rest: rest, span, Prefix.not]
			else => return This[parseExpr1: tokens]
		}
	}

	on [parseExpr2Rest: tokens (Tokens), span (Span), op (Prefix)] (Result[Expr]) {
		match This[parseExpr2: tokens] {
			at Result[success: my right, my rest] => return Result[success: Expr[prefix: span, op :right], rest]
			at my fail => return fail
		}
	}


	;[
	==| #tag expr
	]
	on [parseExpr3: tokens (Tokens)] (Result[Expr]) {
		match tokens {
			at #[Token[span: my span tag: my name], ...my rest] => match This[parseExpr3: rest] {
				at Result[success: my expr, my rest'] {
					my tag = Ident[:span :name]
					return Result[success: Expr[:tag :expr], rest']
				}
				at my fail => return fail[fatalIfBad: rest]
			}
			else => return This[parseExpr2: tokens]
		}
	}


	;[
	==| Type -> ...
	==| sender -> ...
	]

	on [parseExpr4: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr3: tokens] {
			at Result[success: my expr, #[Token[span: my span cascade: 1], ...my rest]] {
				match expr at Expr[type: my type] {
					match This[Type parseExpr4Contents: rest, span] {
						at Result[success: my cascades, my rest'] => return Result[success: Expr[:type :cascades], rest']
						at my fail => return fail[Result[Expr]]
					}
				} else {
					match This[Expr parseExpr4Contents: rest, span] {
						at Result[success: my cascades, my rest'] => return Result[success: Expr[:expr :cascades], rest']
						at my fail => return fail[Result[Expr]]
					}
				}
			}
			at my result => return result
		}
	}

	type T if T ?= Type || T ?= Expr
	category T {
		on [parseExpr4Contents: tokens (Tokens), span (Span)] (Result[Cascades[T]]) is static {
			my cascades = #[]
			my rest = tokens
			
			while true {
				match This[T parseExpr4CascadeContents: tokens, span, 1] {
					at Result[success: my cascade, rest = _] => cascades[add: cascade]
					at my fail => return fail[Result[Cascades[T]]]
				}

				match rest at #[Token[span: span = _ cascade: 1], ...rest = _] {
					next
				} else {
					break
				}
			}

			return Result[success: cascades, rest]
		}

		on [parseExpr4CascadeContents: tokens (Tokens), span (Span), level (Int)] (Result[Cascades[T]]) is static {
			my rest = tokens
			my cascade ;[(Cascade[T])], match tokens {
				at #[Token[lBracket], ...my rest'] => match This[T finishMsg: rest'] {
					at Result[success: #{my message ;[temp] (Message[T]), _}, my rest''] => match rest'' {
						at #[Token[plusPlus: my span'], ...rest = _] {
							cascade = Cascade[T][:span :level :message step: span', Step.incr]
						}
						at #[Token[minusMinus: my span'], ...rest = _] {
							cascade = Cascade[T][:span :level :message step: span', Step.decr]
						}
						at #[Maybe[the: #{my assign, my op}] = _[maybeAssignableOp], ...my rest'''] {
							match This[parseExpr3: rest'''] {
								at Result[success: my expr, rest = _] {
									cascade = Cascade[T][:span :level :message :assign :op :expr]
								}
								at my fail => return fail[Result[Cascades[T]]]
							}
						}
						else {
							rest = rest''
							cascade = Cascade[T][:span :level :message]
						}
					}
					at my fail => return fail[Result[Cascades[T]]]
				}

				at #[Token[lBrace], ..._] => match This[parseBlock: tokens] {
					at Result[success: my block, rest = _] => cascade = Cascade[T][:span :level :block]
					at my fail => return fail[Result[Cascades[T]]]
				}

				at #[Token[span: my span' name: my name] = _[asAnyName], ...my rest'] {
					my member = Ident #{span', name}
					
					match rest' {
						at #[Token[plusPlus: my span''], ...rest = _] {
							cascade = Cascade[T][:span :level :member step: span'', Step.incr]
						}
						at #[Token[minusMinus: my span''], ...rest = _] {
							cascade = Cascade[T][:span :level :member step: span'', Step.decr]
						}
						at #[Maybe[the: #{my assign, my op}] = _[maybeAssignableOp], ...my rest''] {
							match This[parseExpr3: rest''] {
								at Result[success: my expr, rest = _] {
									cascade = Cascade[T][:span :level :member :assign :op :expr]
								}
								at my fail => return fail[Result[Cascades[T]]]
							}
						}
						else {
							rest = rest'
							cascade = Cascade[T][:span :level :member]
						}
					}
				}

				else => return Result[fatal: tokens, Maybe[none]]
			}
			my level' = level + 1

			while true {
				match rest at #[Token[span: my span' cascade: level'], ...my rest'] {
					match This[Expr parseExpr4CascadeContents: rest', span', level'] {
						at Result[success: my nested, rest = _] => cascade.nested[add: nested]
						at my fail => return fail[fatalIfFailed]
					}
				} else {
					break
				}
			}

			return Result[success: cascade, rest]
		}
	}


	;[
	==| left ** right
	]

	on [parseExpr5: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr4: tokens] {
			at Result[success: my left, #[Token[starStar: my span], ...my rest]] => match This[parseExpr5: rest] {
				at Result[success: my right, my rest'] => return Result[success: Expr[:left infix: span, Infix[pow] :right], rest']
				at my fail => return fail[fatalIfBad: rest]
			}
			at my result => return result
		}
	}

	
	;[
	==| left * right
	==| left / right
	==| left // right
	==| left % right
	]

	on [parseExpr6: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr5: tokens] {
			at Result[success: my left, my rest] {
				while true {
					match {
						match rest {
							at #[Token[star: my span], ..._] => return Maybe[the: #{Infix[times], span}]
							at #[Token[div: my span], ..._] => return Maybe[the: #{Infix[div], span}]
							at #[Token[divDiv: my span], ..._] => return Maybe[the: #{Infix[intDiv], span}]
							at #[Token[mod: my span], ..._] => return Maybe[the: #{Infix[mod], span}]
							else => return Maybe[none]
						}
					} at Maybe[the: #{my op, my span}] {
						match This[parseExpr5: rest[next]] {
							at Result[success: my right, rest = _] => left = Expr[:left infix: span, op :right]
							at my fail => return fail[fatalIfBad: rest]
						}
					} else {
						return Result[success: left, rest]
					}
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| left + right
	==| left - right
	]

	on [parseExpr7: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr6: tokens] {
			at Result[success: my left, my rest] {
				while true {
					match {
						match rest {
							at #[Token[plus: my span], ..._] => return Maybe[the: #{Infix[plus], span}]
							at #[Token[minus: my span], ..._] => return Maybe[the: #{Infix[minus], span}]
							else => return Maybe[none]
						}
					} at Maybe[the: #{my op, my span}] {
						match This[parseExpr6: rest[next]] {
							at Result[success: my right, rest = _] => left = Expr[:left infix: span, op :right]
							at my fail => return fail[fatalIfBad: rest]
						}
					} else {
						return Result[success: left, rest]
					}
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| left & right
	==| left | right
	==| left ^ right
	==| left << right
	==| left >> right
	]

	on [parseExpr8: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr7: tokens] {
			at Result[success: my left, my rest] {
				while true {
					match {
						match rest {
							at #[Token[and: my span], ..._] => return Maybe[the: #{Infix[bitAnd], span}]
							at #[Token[bar: my span], ..._] => return Maybe[the: #{Infix[bitOr], span}]
							at #[Token[caret: my span], ..._] => return Maybe[the: #{Infix[bitXor], span}]
							at #[Token[ltLt: my span], ..._] => return Maybe[the: #{Infix[shl], span}]
							at #[Token[gtGt: my span], ..._] => return Maybe[the: #{Infix[shr], span}]
							else => return Maybe[none]
						}
					} at Maybe[the: #{my op, my span}] {
						match This[parseExpr7: rest[next]] {
							at Result[success: my right, rest = _] => left = Expr[:left infix: span, op :right]
							at my fail => return fail[fatalIfBad: rest]
						}
					} else {
						return Result[success: left, rest]
					}
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| left %% right
	]

	on [parseExpr9: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr8: tokens] {
			at Result[success: my left, my rest] {
				match {
					match rest {
						at #[Token[modMod: my span], ..._] => return Maybe[the: #{Infix[isMod], span}]
						else => return Maybe[none]
					}
				} at Maybe[the: #{my op, my span}] {
					match This[parseExpr9: rest[next]] {
						at Result[success: my right, my rest'] => return Result[success: Expr[:left infix: span, op :right], rest']
						at my fail => return fail[fatalIfBad: rest]
					}
				} else {
					return Result[success: left, rest]
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| left ?= right
	==| left != right
	==| left > right
	==| left >= right
	==| left < right
	==| left <= right
	]

	on [parseExpr10: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr9: tokens] {
			at Result[success: my left, my rest] {
				while true {
					match {
						match rest {
							at #[Token[questionEq: my span], ..._] => return Maybe[the: #{Infix[eq], span}]
							at #[Token[bangEq: my span], ..._] => return Maybe[the: #{Infix[ne], span}]
							at #[Token[gt: my span], ..._] => return Maybe[the: #{Infix[gt], span}]
							at #[Token[gtEq: my span], ..._] => return Maybe[the: #{Infix[ge], span}]
							at #[Token[lt: my span], ..._] => return Maybe[the: #{Infix[lt], span}]
							at #[Token[ltEq: my span], ..._] => return Maybe[the: #{Infix[le], span}]
							else => return Maybe[none]
						}
					} at Maybe[the: #{my op, my span}] {
						match This[parseExpr9: rest[next]] {
							at Result[success: my right, rest = _] => left = Expr[:left infix: span, op :right]
							at my fail => return fail[fatalIfBad: rest]
						}
					} else {
						return Result[success: left, rest]
					}
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| left && right
	==| left || right
	==| left ^^ right
	==| left !! right
	]

	on [parseExpr11: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr10: tokens] {
			at Result[success: my left, my rest] {
				while true {
					match {
						match rest {
							at #[Token[andAnd: my span], ..._] => return Maybe[the: #{Infix[and], span}]
							at #[Token[barBar: my span], ..._] => return Maybe[the: #{Infix[or], span}]
							at #[Token[caretCaret: my span], ..._] => return Maybe[the: #{Infix[xor], span}]
							at #[Token[bangBang: my span], ..._] => return Maybe[the: #{Infix[nor], span}]
							else => return Maybe[none]
						}
					} at Maybe[the: #{my op, my span}] {
						match This[parseExpr10: rest[next]] {
							at Result[success: my right, rest = _] => left = Expr[:left infix: span, op :right]
							at my fail => return fail[fatalIfBad: rest]
						}
					} else {
						return Result[success: left, rest]
					}
				}
			}
			at my fail => return fail
		}
	}


	;[
	==| lhs = expr
	==| lhs op= expr
	]

	on [parseExpr12: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr11: tokens] {
			at Result[success: my left, #[Maybe[the: #{my span, my op}] = _[maybeAssignableOp], ...my rest]] {
				match This[parseExpr12: rest] {
					at Result[success: my right, my rest'] {
						my op' = {
							match op at Maybe[the: my assign] {
								return Infix[:assign]
							} else {
								return Infix[assign]
							}
						}
						return Result[success: Expr[:left infix: span, op' :right], rest']
					}
					at my fail => return fail[fatalIfBad: rest]
				}
			}
			at my result => return result
		}
	}


	;[
	==| ...expr
	]
	
	on [parseExpr13: tokens (Tokens)] (Result[Expr]) {
		match tokens at #[Token[dotDotDot: my span], ...my rest] {
			match This[parseExpr12: rest] {
				at Result[success: my right, my rest'] => return Result[success: Expr[prefix: span, Prefix.spread :right], rest']
				at my fail => return fail
			}
		} else {
			return This[parseExpr12: tokens]
		}
	}


	;[
	==| Type
	==| -> ...
	==|
	==| expr
	==| -> ...
	]

	on [parseFullExpr: tokens (Tokens)] (Result[Expr]) {
		match This[parseExpr: tokens] {
			at Result[success: my expr, #[Token[lSep], Token[span: my span cascade: 1], ...my rest]] {
				match expr at Expr[type: my type] {
					match This[Type parseExprCascades: rest, span] {
						at Result[success: my cascades, my rest'] => return Result[success: Expr[:type :cascades], rest']
						at my fail => return fail[Result[Expr]]
					}
				} else {
					match This[Expr parseExprCascades: rest, span] {
						at Result[success: my cascades, my rest'] => return Result[success: Expr[:expr :cascades], rest']
						at my fail => return fail[Result[Expr]]
					}
				}
			}
			at my result => return result
		}
	}

	;@@ TODO: make this more modular, duplicate code bad
	type T if T ?= Expr || T ?= Type
	category T {
		on [parseExprCascades: tokens (Tokens), span (Span)] (Result[Cascades[T]]) is static {
			my cascades = #[]
			my rest = tokens
			
			while true {
				match This[T parseExprCascadeContents: tokens, span, 1] {
					at Result[success: my cascade, rest = _] => cascades[add: cascade]
					at my fail => return fail[Result[Cascades[T]]]
				}

				match rest at #[Token[lSep], Token[span: span = _ cascade: 1], ...rest = _] {
					next
				} else {
					break
				}
			}

			return Result[success: cascades, rest]
		}

		on [parseExprCascadeContents: tokens (Tokens), span (Span), level (Int)] (Result[Cascades[T]]) is static {
			my rest = tokens
			my cascade ;[(Cascade[T])], match tokens {
				at #[Token[lBracket], ...my rest'] => match This[T finishMsg: rest'] {
					at Result[success: #{my message ;[temp] (Message[T]), _}, my rest''] => match rest'' {
						at #[Token[plusPlus: my span'], ...rest = _] {
							cascade = Cascade[T][:span :level :message step: span', Step.incr]
						}
						at #[Token[minusMinus: my span'], ...rest = _] {
							cascade = Cascade[T][:span :level :message step: span', Step.decr]
						}
						at #[Maybe[the: #{my assign, my op}] = _[maybeAssignableOp], ...my rest'''] {
							match This[parseExpr: rest'''] {
								at Result[success: my expr, rest = _] {
									cascade = Cascade[T][:span :level :message :assign :op :expr]
								}
								at my fail => return fail[Result[Cascades[T]]]
							}
						}
						else {
							rest = rest''
							cascade = Cascade[T][:span :level :message]
						}
					}
					at my fail => return fail[Result[Cascades[T]]]
				}

				at #[Token[lBrace], ..._] => match This[parseBlock: tokens] {
					at Result[success: my block, rest = _] => cascade = Cascade[T][:span :level :block]
					at my fail => return fail[Result[Cascades[T]]]
				}

				at #[Token[span: my span' name: my name] = _[asAnyName], ...my rest'] {
					my member = Ident #{span', name}
					
					match rest' {
						at #[Token[plusPlus: my span''], ...rest = _] {
							cascade = Cascade[T][:span :level :member step: span'', Step.incr]
						}
						at #[Token[minusMinus: my span''], ...rest = _] {
							cascade = Cascade[T][:span :level :member step: span'', Step.decr]
						}
						at #[Maybe[the: #{my assign, my op}] = _[maybeAssignableOp], ...my rest''] {
							match This[parseExpr: rest''] {
								at Result[success: my expr, rest = _] {
									cascade = Cascade[T][:span :level :member :assign :op :expr]
								}
								at my fail => return fail[Result[Cascades[T]]]
							}
						}
						else {
							rest = rest'
							cascade = Cascade[T][:span :level :member]
						}
					}
				}

				else => return Result[fatal: tokens, Maybe[none]]
			}
			my level' = level + 1

			while true {
				match rest at #[Token[lSep], Token[span: my span' cascade: level'], ...my rest'] {
					match This[Expr parseExprCascadeContents: rest', span', level'] {
						at Result[success: my nested, rest = _] => cascade.nested[add: nested]
						at my fail => return fail[fatalIfFailed]
					}
				} else {
					break
				}
			}

			return Result[success: cascade, rest]
		}
	}
}