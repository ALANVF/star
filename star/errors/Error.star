use Info from: Diagnostic
use Priority from: Info

use Token from: Lexer
;use Operator from: Parser.Decl as: UOperator
;use NamedDecl from: Parser.Decl as: UNamedDecl
;use Decl from: Parser as: UDecl
use Typer
alias UOperator is hidden = Parser.Decl.Operator
alias UNamedDecl is hidden = Parser.Decl.NamedDecl
alias UDecl is hidden = Parser.Decl

protocol Error {
	my diag (Diagnostic)
}

kind LexError of Error {
	has [unterminatedComment: begin (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: #[
				Info[
					span: begin
					message: "Unterminated comment"
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidOperator: name (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: #[
				Info[
					:span
					message: "Invalid operator `\(name)`"
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidInput: input (Char), span (Span) afterHash: begin (Span)] {
		diag = Diagnostic[
            severity: Severity.error
			message: "Syntax error"
			info: #[
				Info[
					:span
					message: "Unexpected `\(input[escape])` after `#`"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
        ]
	}

	has [invalidEqEq: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: #[
				Info[
					:span
					message: "Please use `?=` instead of `==` in Star"
					priority: Priority.primary
				]
			]
		]
	}

	has [unterminatedCascade: begin (Span), end (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unterminated cascade"
			info: #[
				Info[
					span: end
					message: "Expected a `>` to finish the cascade operator"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidInput: span (Span)] {
		diag = Diagnostic[
            severity: Severity.error
            message: "Syntax error"
            info: #[
                Info[
                    :span
                    message: "This is not the syntax that you are looking for"
                    priority: Priority.primary
                ]
            ]
        ]
	}

	has [invalidHexStart: begin (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unexpected start of hexdecimal literal"
			info: #[
				Info[
					span: begin
					message: "Were you wanting a hexdecimal literal here or what?"
					priority: Priority.primary
				]
			]
		]
	}
	
	has [name: (Span) afterHex: hex (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid hexdecimal literal"
			info: #[
				Info[
					span: name
					message: "Make sure to separate names from numbers"
					priority: Priority.primary
				]
				Info[
					span: hex
					priority: Priority.secondary
				]
			]
		]
	}

	has [incompleteDecimalPoint: int (Span), point (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid decimal literal"
			info: #[
				Info[
					span: point
					message: "At least 1 digit is required on both sides of the decimal point"
					priority: Priority.primary
				]
				Info[
					span: int
					priority: Priority.secondary
				]
			]
		]
	}

	has [name: (Span) afterNumber: num (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid number literal"
			info: #[
				Info[
					span: name
					message: "Make sure to separate names from numbers"
					priority: Priority.primary
				]
				Info[
					span: num
					priority: Priority.secondary
				]
			]
		]
	}

	has [missingExponent: e (Span), exp (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid number literal"
			info: #[
				Info[
					span: exp
					message: "Expected a number after the exponent indicator"
					priority: Priority.primary
				]
				Info[
					span: e
					message: "This indicates that the number has an exponent"
					priority: Priority.secondary
				]
			]
		]
	}

	has [noUppercasePunnedLabel: begin (Span), head (Span), rest (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid punned label"
			info: #[
				Info[
					span: head
					message: "Punned labels may not start with an uppercase letter"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
				Info[
					span: rest
					priority: Priority.secondary
				]
			]
		]
	}

	has [incompletePunnedLabel: begin (Span), name (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid punned label"
			info: #[
				Info[
					span: name
					message: "Was expecting a name for the punned label"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [noUppercaseLabel: head (Span), rest (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid label"
			info: #[
				Info[
					span: head
					message: "Labels may not start with an uppercase letter"
					priority: Priority.primary
				]
				Info[
					span: rest
					priority: Priority.secondary
				]
			]
		]
	}

	has [escapeCharQuote: begin (Span), quote (Span), end (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid char literal"
			info: #[
				Info[
					span: quote
					message: "`\"` characters need to be escaped in char literals"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
				Info[
					span: end
					priority: Priority.secondary
				]
			]
		]
	}

	has [noEmptyChar: char (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid char literal"
			info: #[
				Info[
					span: char
					message: "Char literals may not be empty"
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidCharEscape: begin (Span), char (Char), span (Span), end (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid escape character"
			info: #[
				; off by 1 errors?
				Info[
					:span
					message: "Escape character `\(char)` " + {
						if char ?= #"(" {
							return "is not allowed in char literals"
						} else {
							return "does not exist"
						}
					}
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
				Info[
					span: end
					priority: Priority.secondary
				]
			]
		]
	}

	has [unterminatedChar: begin (Span), end (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unterminated char literal"
			info: #[
				Info[
					span: end
					message: "Expected another `\"` to finish the char literal"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidHexEscape: begin (Span), esc (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid hexdecimal escape code"
			info: #[
				Info[
					span: esc
					message: "Was expecting a hexdecimal digit here"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidUniEscape: begin (Span), esc (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid unicode escape code"
			info: #[
				Info[
					span: esc
					message: "Was expecting a hexdecimal digit here"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidOctEscape: begin (Span), esc (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid octal escape code"
			info: #[
				Info[
					span: esc
					message: "Was expecting an octal digit here"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidStrEscape: char (Char), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid escape character"
			info: #[
				Info[
					:span
					message: "Escape character `\\\(char)` does not exist"
					priority: Priority.primary
				]
			]
		]
	}

	has [unterminatedStr: begin (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unterminated string"
			info: #[
				Info[
					span: begin
					message: "This string is never terminated"
					priority: Priority.primary
				]
			]
		]
	}

	has [name: (Span), afterAnonArg: arg (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid anonymous argument"
			info: #[
				Info[
					span: name
					message: "Make sure to separate names from numbers"
					priority: Priority.primary
				]
				Info[
					span: arg
					priority: Priority.secondary
				]
			]
		]
	}

	has [unterminatedAnonArg: begin (Span), end (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unterminated anonymous argument"
			info: #[
				Info[
					span: end
					message: "Was expecting a number here"
					priority: Priority.primary
				]
				Info[
					span: begin
					priority: Priority.secondary
				]
			]
		]
	}
}

kind ParseError of Error {
	has [unexpectedTokenWantedSep: token (Token)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: #[
				Info[
					span: token.span
					message: "Unexpected \(token.basicName), was expecting a comma or newline instead"
					priority: Priority.primary
				]
			]
		]
	}
	
	has [unexpectedToken: first (Token), last (Maybe[Token])] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: {
				match last at Maybe[the: my last'] if last' != first {
					return #[
						Info[
							span: last'.span
							message: "Unexpected \(last'.basicName)"
							priority: Priority.primary
						]
						Info[
							span: first.span
							message: "Starting here"
							priority: Priority.secondary
						]
					]
				} else {
					return #[
						Info[
							span: first.span
							message: "Unexpected \(first.basicName)"
							priority: Priority.primary
						]
					]
				}
			}
		]
	}

	has [unexpectedEOF: first (Token), last (Token)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Syntax error"
			info: {
				if first ?= last {
					return #[
						Info[
							span: last.span
							message: "Unexpected end of file after \(last.basicName)"
							priority: Priority.primary
						]
					]
				} else {
					return #[
						Info[
							span: last.span
							message: "Unexpected end of file after \(last.basicName)"
							priority: Priority.primary
						]
						Info[
							span: first.span
							message: "Starting here"
							priority: Priority.secondary
						]
					]
				}
			}
		]
	}

	has [noGenericMember: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid member"
			info: #[
				Info[
					:span
					message: "Members are not allowed to be generic"
					priority: Priority.primary
				]
			]
		]
	}

	has [noGenericCase: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid case"
			info: #[
				Info[
					:span
					message: "Cases are not allowed to be generic"
					priority: Priority.primary
				]
			]
		]
	}

	has [noGenericDeinit: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid deinitializer"
			info: #[
				Info[
					:span
					message: "Deinitializers are not allowed to be generic"
					priority: Priority.primary
				]
			]
		]
	}
}

kind TypeError of Error {
	has [tooManyErrors] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Too many errors!"
			info: #[ ]
		]
	}

	has [unorganizedCode: span (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Unorganized code"
			info: #[
				Info[
					:span
					message: "All imports should be at the beginning of the file"
					priority: Priority.secondary
				]
			]
		]
	}

	has [unknownPragma: pragma (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown pragma"
			info: #[
				Info[
					:span
					message: "Unknown pragma `\(pragma)`"
					priority: Priority.primary
				]
			]
		]
	}

	has [redundantGetter: member (Str), span (Span), getter (Str), span' (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Redundant code"
			info: #[
				Info[
					span: span
					message: "Unnecessary use of \"is getter `\(getter)`\". Doing \"is getter\" is just fine"
					priority: Priority.primary
				]
				Info[
					span: span'
					message: "For member `\(member)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [redundantSetter: member (Str), span (Span), setter (Str), span' (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Redundant code"
			info: #[
				Info[
					span: span
					message: "Unnecessary use of \"is setter `\(setter)`\". Doing \"is setter\" is just fine"
					priority: Priority.primary
				]
				Info[
					span: span'
					message: "For member `\(member)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [redundantGetterSetter: member (Str), span (Span), getter (Span), setter (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Redundant code"
			info: #[
				Info[
					span: getter
					message: "Unnecessary use of \"is getter\" along with \"is setter\""
					priority: Priority.primary
				]
				Info[
					span: setter
					priority: Priority.primary
				]
				Info[
					:span
					message: "For member `\(member)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [opNotOverloadable: decl (AnyTypeDecl), op (UOperator) yet: (Bool) = false] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid operator overload"
			info: #[
				Info[
					span: op.symbol.span
					message: "The `\(op.symbol.name)` operator cannot be overloaded" -> { if yet => this[add: " (yet)"] }
					priority: Priority.primary
				]
				Info[
					span: op.span
					priority: Priority.secondary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(decl.fullName)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [opNeedsParameter: decl (AnyTypeDecl), op (UOperator)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid operator overload"
			info: #[
				Info[
					span: op.symbol.span
					message: "Overloading the `\(op.symbol.name)` operator requires a parameter"
					priority: Priority.primary
				]
				Info[
					span: op.span
					priority: Priority.secondary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(decl.fullName)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [opDoesNotNeedParameter: decl (AnyTypeDecl), op (UOperator)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid operator overload"
			info: #[
				Info[
					span: op.symbol.span
					message: "Overloading the `\(op.symbol.name)` operator should not require a parameter"
					priority: Priority.primary
				]
				Info[
					span: op.span
					priority: Priority.secondary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(decl.fullName)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [unknownOpOverload: decl (AnyTypeDecl), op (UOperator)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid operator overload"
			info: #[
				Info[
					span: op.symbol.span
					message: "The `\(op.symbol.name)` operator cannot be overloaded because it does not exist"
					priority: Priority.primary
				]
				Info[
					span: op.span
					priority: Priority.secondary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(decl.fullName)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [noTaggedKindRepr: kind (TaggedKind), repr (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid declaration"
			info: #[
				Info[
					span: repr
					message: "Tagged kinds may not have an underlaying type"
					priority: Priority.primary
				]
				Info[
					span: kind.span
					message: "For kind `\(kind.name)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [noValueCaseInit: vcase (Str), span (Span), init (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid value case"
			info: #[
				Info[
					span: init
					message: "Value cases may not have an initializer"
					priority: Priority.primary
				]
				Info[
					:span
					message: "For value case `\(vcase)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [duplicateAttribute: decl (Decl), name (Str), attr (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Duplicate attribute"
			info: #[
				Info[
					:span
					message: "Duplicate attribute `is \(attr)`"
					priority: Priority.primary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(name)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [invalidAttribute: decl (Decl), name (Str), attr (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid attribute"
			info: #[
				Info[
					:span
					message: "Invalid attribute `is \(attr)`"
					priority: Priority.primary
				]
				Info[
					span: decl.span
					message: "For \(decl.declName) `\(name)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [duplicateDecl: decl' (UNamedDecl) inDecl: decl (AnyTypeDecl)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Duplicate declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Duplicate \(decl'.name)"
					priority: Priority.primary
				]
				Info[
					span: decl.span
					message: "In \(decl.declName) `\(decl.name)`"
					priority: Priority.secondary
				]
			]
		]
	}
	has [duplicateDecl: decl' (UNamedDecl) inFile: file (File)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Duplicate declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Duplicate \(decl'.name)"
					priority: Priority.primary
				]
			]
		]
	}

	has [unexpectedDecl: decl' (UNamedDecl) inDecl: decl (AnyTypeDecl)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unexpected declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Unexpected \(decl'.name)"
					priority: Priority.primary
				]
				Info[
					span: decl.span
					message: "In \(decl.declName) `\(decl.name)`"
					priority: Priority.secondary
				]
			]
		]
	}
	has [unexpectedDecl: decl' (UNamedDecl) inFile: file (File)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unexpected declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Unexpected \(decl'.name)"
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidDecl: decl' (UNamedDecl) inDecl: decl (AnyTypeDecl)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Invalid \(decl'.name)"
					priority: Priority.primary
				]
				Info[
					span: decl.span
					message: "In \(decl.declName) `\(decl.name)`"
					priority: Priority.secondary
				]
			]
		]
	}
	has [invalidDecl: decl' (UNamedDecl) inFile: file (File)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid declaration"
			info: #[
				Info[
					span: decl'.span
					message: "Invalid \(decl'.name)"
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidTypeLookup: span (Span) why: (Str) = "Invalid type lookup"] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid type lookup"
			info: #[
				Info[
					:span
					message: why
					priority: Priority.primary
				]
			]
		]
	}

	has [invalidTypeApply: span (Span) why: (Str) = "Invalid type application"] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid type application"
			info: #[
				Info[
					:span
					message: why
					priority: Priority.primary
				]
			]
		]
	}

	has [notYetImplemented: span (Span) why: (Str) = "This feature has not been implemented yet"] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Not yet implemented"
			info: #[
				Info[
					:span
					message: why
					priority: Priority.primary
				]
			]
		]
	}

	has [method: (RealMethod) duplicateParam: name (Str), origSpan (Span), dupSpan (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Duplicate parameter"
			info: #[
				Info[
					span: dupSpan
					message: "Duplicate parameter `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: origSpan
					message: "First defined here"
					priority: Priority.secondary
				]
				Info[
					span: method.span
					message: "For \(method.declName) `\(method.methodName)`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [taggedCase: tcase (TaggedCase.Multi) duplicateParam: name (Str), origSpan (Span), dupSpan (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Duplicate parameter"
			info: #[
				Info[
					span: dupSpan
					message: "Duplicate parameter `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: origSpan
					message: "First defined here"
					priority: Priority.secondary
				]
				Info[
					span: tcase.span
					message: "For \(tcase.declName) `\(tcase.params[displayLabels])`"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) unknownFieldOrVar: name (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown name"
			info: #[
				Info[
					:span
					message: "Unknown field or variable `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) shadowedLocalVar: name (Str), origSpan (Span), dupSpan (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Shadowed variable"
			info: #[
				Info[
					span: dupSpan
					message: "This shadows an existing local variable `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: origSpan
					message: "First defined here"
					priority: Priority.secondary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) localVarTypeMismatch: name (Str), gotType (Type), wantedType (Type), declSpan (Span), hereSpan (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Type mismatch"
			info: #[
				Info[
					span: hereSpan
					message: "local variable `\(name)` declared to be of type `\(wantedType.fullName)`, but got `\(gotType.fullName)` instead"
					priority: Priority.primary
				]
				Info[
					span: declSpan
					message: "First defined here"
					priority: Priority.secondary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) sender: (Type) unknownMethod: kind (MethodKind), span (Span) categories: (Array[Category]) = #[]] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown method"
			info: #[
				Info[
					:span
					message: {
						#{my access, my methodName} = kind[accessAndName]
						my msg = "\(access.desc) `\(sender.fullName)` does not respond to method \(methodName)"
						
						if categories? {
							msg[add: " in any categories of:"]
							for my cat in: categories {
								msg[add: "\n    \(cat.fullName)"]
							}
						}
						
						return msg
					}
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) sender: (Type) unknownCast: target (Type), span (Span) categories: (Array[Category]) = #[]] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown cast"
			info: #[
				Info[
					:span
					message: {
						my msg = "Value of type `\(sender.fullName)` cannot be cast to type `\(target.fullName)`"
						
						if categories? {
							msg[add: " in any categories of:"]
							for my cat in: categories {
								msg[add: "\n    \(cat.fullName)"]
							}
						}
	
						return msg
					}
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) sender: (Type) access: (Access) unknownGetter: name (Str), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown name access"
			info: #[
				Info[
					:span
					message: "\(access.desc) `\(sender.fullName)` does not have member/getter `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) sender: (Type) access: (Access) unknownSetter: name (Str), span (Span) value: (Maybe[Expr])] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown name access"
			info: #[
				Info[
					:span
					message: {
						my msg = "\(access.desc) `\(sender.fullName)` does not have member/setter `\(name)`"
	
						match value at Maybe[the: my expr] {
							msg[add: " of type \({
								match expr.t at Maybe[the: my t] {
									return t.fullName
								} else {
									return "???"
								}
							})"]
						}
	
						return msg
					}
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) sender: (Type) access: (Access) unknownCategory: cat (Category), span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Unknown cast"
			info: #[
				Info[
					:span
					message: "\(access.desc) `\(sender.fullName)` does not have the category `\(cat.fullName)`"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) thisNotAllowed: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid usage"
			info: #[
				Info[
					:span
					message: "`this` is not allowed in a static context"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) expectedLogicalValue: span (Span) butGot: gotType (Type)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid type"
			info: #[
				Info[
					:span
					message: "Expected a logical value, but got value of type `\(gotType.fullName)` instead"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) possiblyUnintendedArrowBlock: span (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Possibly unintentional arrow shorthand"
			info: #[
				Info[
					:span
					message: "Using a block in an arrow shorthand does not act the same as a plain block!"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) arrayPatternNotAllowed: span (Span)] {
		diag = Diagnostic[
			severity: Severity.error
			message: "Invalid pattern"
			info: #[
				Info[
					:span
					message: "This pattern is only allowed in array patterns"
					priority: Priority.primary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}

	has [ctx: (Ctx) duplicateBinding: name (Str), origSpan (Span), dupSpan (Span)] {
		diag = Diagnostic[
			severity: Severity.warning
			message: "Duplicate binding"
			info: #[
				Info[
					span: dupSpan
					message: "This shadows a previous binding `\(name)`"
					priority: Priority.primary
				]
				Info[
					span: origSpan
					message: "First defined here"
					priority: Priority.secondary
				]
				Info[
					span: ctx.typeLookup.span
					message: "In \(ctx.description)"
					priority: Priority.secondary
				]
			]
		]
	}
}