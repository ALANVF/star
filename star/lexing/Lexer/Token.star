kind Token {
	my span (Span)

	has [cSep]
	has [comma]
	has [lSep]

	has [module]
	has [my]
	has [on]
	has [return]
	has [init]
	has [deinit]
	has [operator]
	has [class]
	has [alias]
	has [type]
	has [kind]
	has [category]
	has [protocol]
	has [is]
	has [of]
	has [use]
	has [has]
	has [if]
	has [orif]
	has [else]
	has [while]
	has [for]
	has [do]
	has [case]
	has [match]
	has [at]
	has [break]
	has [next]
	has [throw]
	has [try]
	has [catch]

	has [static]
	has [hidden]
	has [readonly]
	has [friend]
	has [sealed]
	has [unordered]
	has [getter]
	has [setter]
	has [main]
	has [inline]
	has [noinherit]
	has [pattern]
	has [asm]
	has [native]
	has [flags]
	has [uncounted]
	has [strong]
	has [sealed]
	has [macro]

	has [tilde]
	has [dot]
	has [eq]
	has [eqGt]
	has [plus]
	has [plusEq]
	has [plusPlus]
	has [minus]
	has [minusEq]
	has [minusMinus]
	has [star]
	has [starEq]
	has [starStar]
	has [starStarEq]
	has [div]
	has [divEq]
	has [divDiv]
	has [divDivEq]
	has [mod]
	has [modEq]
	has [modMod]
	has [modModEq]
	has [and]
	has [andEq]
	has [andAnd]
	has [andAndEq]
	has [bar]
	has [barEq]
	has [barBar]
	has [barBarEq]
	has [caret]
	has [caretEq]
	has [caretCaret]
	has [caretCaretEq]
	has [bang]
	has [bangEq]
	has [bangBang]
	has [bangBangEq]
	has [question]
	has [questionEq]
	has [gt]
	has [gtEq]
	has [gtGt]
	has [gtGtEq]
	has [lt]
	has [ltEq]
	has [ltLt]
	has [ltLtEq]
	has [dotDotDot]
	has [cascade: depth (Int)]

	has [lParen: span (Span)]       => [lParen]       { this.span = span }
	has [lBracket: span (Span)]     => [lBracket]     { this.span = span }
	has [lBrace: span (Span)]       => [lBrace]       { this.span = span }
	has [hashLParen: span (Span)]   => [hashLParen]   { this.span = span }
	has [hashLBracket: span (Span)] => [hashLBracket] { this.span = span }
	has [hashLBrace: span (Span)]   => [hashLBrace]   { this.span = span }
	has [rParen: span (Span)]       => [rParen]       { this.span = span }
	has [rBracket: span (Span)]     => [rBracket]     { this.span = span }
	has [rBrace: span (Span)]       => [rBrace]       { this.span = span }

	has [name: (Str)]
	has [typeName: (Str)]
	has [label: (Str)]
	has [punned: (Str)]
	has [tag: (Str)]
	has [litsym: (Str)]

	has [int: (Str) exp: (Maybe[Str])]
	has [int: (Str) dec: (Str) exp: (Maybe[Str])]
	has [hex: (Str)]
	has [str: (Array[StrSegment])]
	has [char: (Char)]
	has [bool: (Bool)]
	has [this]
	has [wildcard]
	has [anonArg: nth (Int) depth: (Int)]

	on [basicName] (Str) is getter {
		match this {
			at Token[cSep] || Token[comma] => return "comma"
			at Token[module] <= _ <= Token[catch] => return "keyword"
			at Token[static] <= _ <= Token[macro] => return "attribute"
			at Token[dot] => return "dot"
			at Token[tilde] <= _ <= Token[dotDotDot] => return "operator"
			at Token[cascade: _] => return "cascade"
			at Token[lParen] => return "opening parenthesis"
			at Token[lBracket] => return "opening bracket"
			at Token[lBrace] => return "opening brace"
			at Token[hashLParen] => return "hash and opening parenthesis"
			at Token[hashLBracket] => return "hash and opening bracket"
			at Token[hashLBrace] => return "hash and opening brace"
			at Token[rParen] => return "closing parenthesis"
			at Token[rBracket] => return "closing bracket"
			at Token[rBrace] => return "closing brace"
			at Token[name: _] => return "name"
			at Token[typeName: _] => return "type name"
			at Token[label: _] => return "label"
			at Token[punned: _] => return "punned label"
			at Token[tag: _] => return "tag"
			at Token[litsym: _] => return "litsym"
			at Token[int: _ exp: _] => return "integer literal"
			at Token[int: _ dec: _ exp: _] => return "decimal literal"
			at Token[hex: _] => return "hexdecimal literal"
			at Token[str: _] => return "string literal"
			at Token[char: _] => return "character literal"
			at Token[bool: _] => return "boolean literal"
			at Token[this] => return "`this` literal"
			at Token[wildcard] => return "wildcard literal"
			at Token[anonArg: _ depth: _] => return "anonymous argument literal"
		}
	}

	on [asSoftName] (Token) {
		match this {
			at Token[module] => return Token[:span name: "module"]
			at Token[on] => return Token[:span name: "on"]
			at Token[init] => return Token[:span name: "init"]
			at Token[deinit] => return Token[:span name: "deinit"]
			at Token[operator] => return Token[:span name: "operator"]
			at Token[class] => return Token[:span name: "class"]
			at Token[alias] => return Token[:span name: "alias"]
			at Token[type] => return Token[:span name: "type"]
			at Token[kind] => return Token[:span name: "kind"]
			at Token[category] => return Token[:span name: "category"]
			at Token[protocol] => return Token[:span name: "protocol"]
			at Token[of] => return Token[:span name: "of"]
			at Token[use] => return Token[:span name: "use"]
			else => return this
		}
	}

	on [asAnyName] (Token) {
		match this {
			at Token[module] => return Token[:span name: "module"]
			at Token[my] => return Token[:span name: "my"]
			at Token[on] => return Token[:span name: "on"]
			at Token[return] => return Token[:span name: "return"]
			at Token[init] => return Token[:span name: "init"]
			at Token[deinit] => return Token[:span name: "deinit"]
			at Token[operator] => return Token[:span name: "operator"]
			at Token[class] => return Token[:span name: "class"]
			at Token[alias] => return Token[:span name: "alias"]
			at Token[type] => return Token[:span name: "type"]
			at Token[kind] => return Token[:span name: "kind"]
			at Token[category] => return Token[:span name: "category"]
			at Token[protocol] => return Token[:span name: "protocol"]
			at Token[is] => return Token[:span name: "is"]
			at Token[of] => return Token[:span name: "of"]
			at Token[use] => return Token[:span name: "use"]
			at Token[has] => return Token[:span name: "has"]
			at Token[if] => return Token[:span name: "if"]
			at Token[orif] => return Token[:span name: "orif"]
			at Token[else] => return Token[:span name: "else"]
			at Token[while] => return Token[:span name: "while"]
			at Token[for] => return Token[:span name: "for"]
			at Token[do] => return Token[:span name: "do"]
			at Token[case] => return Token[:span name: "case"]
			at Token[match] => return Token[:span name: "match"]
			at Token[at] => return Token[:span name: "at"]
			at Token[break] => return Token[:span name: "break"]
			at Token[next] => return Token[:span name: "next"]
			at Token[throw] => return Token[:span name: "throw"]
			at Token[try] => return Token[:span name: "try"]
			at Token[catch] => return Token[:span name: "catch"]
			at Token[bool: true] => return Token[:span name: "true"]
			at Token[bool: false] => return Token[:span name: "false"]
			at Token[this] => return Token[:span name: "this"]
			else => return this
		}
	}
	
	on [isAnySep] (Bool) is inline {
		match this at Token[lSep] || Token[cSep] || Token[comma] {
			return true
		} else {
			return false
		}
	}
	
	on [isAnyComma] (Bool) is inline {
		match this at Token[cSep] || Token[comma] {
			return true
		} else {
			return false
		}
	}
}