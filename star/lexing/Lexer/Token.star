use Assignable from: Parser.Infix

kind Token {
	my span (Span)

	has [cSep]
	has [comma]
	has [lSep]

	has [module: span (Span)]   => [module]   { this.span = span }
	has [my: span (Span)]       => [my]       { this.span = span }
	has [on: span (Span)]       => [on]       { this.span = span }
	has [return: span (Span)]   => [return]   { this.span = span }
	has [init: span (Span)]     => [init]     { this.span = span }
	has [deinit: span (Span)]   => [deinit]   { this.span = span }
	has [operator: span (Span)] => [operator] { this.span = span }
	has [class: span (Span)]    => [class]    { this.span = span }
	has [alias: span (Span)]    => [alias]    { this.span = span }
	has [type: span (Span)]     => [type]     { this.span = span }
	has [kind: span (Span)]     => [kind]     { this.span = span }
	has [category: span (Span)] => [category] { this.span = span }
	has [protocol: span (Span)] => [protocol] { this.span = span }
	has [is: span (Span)]       => [is]       { this.span = span }
	has [of: span (Span)]       => [of]       { this.span = span }
	has [use: span (Span)]      => [use]      { this.span = span }
	has [has: span (Span)]      => [has]      { this.span = span }
	has [if: span (Span)]       => [if]       { this.span = span }
	has [else: span (Span)]     => [else]     { this.span = span }
	has [while: span (Span)]    => [while]    { this.span = span }
	has [for: span (Span)]      => [for]      { this.span = span }
	has [recurse: span (Span)]  => [recurse]  { this.span = span }
	has [do: span (Span)]       => [do]       { this.span = span }
	has [case: span (Span)]     => [case]     { this.span = span }
	has [match: span (Span)]    => [match]    { this.span = span }
	has [at: span (Span)]       => [at]       { this.span = span }
	has [break: span (Span)]    => [break]    { this.span = span }
	has [next: span (Span)]     => [next]     { this.span = span }
	has [throw: span (Span)]    => [throw]    { this.span = span }
	has [try: span (Span)]      => [try]      { this.span = span }
	has [catch: span (Span)]    => [catch]    { this.span = span }

	has [static: span (Span)]    => [static]    { this.span = span }
	has [hidden: span (Span)]    => [hidden]    { this.span = span }
	has [readonly: span (Span)]  => [readonly]  { this.span = span }
	has [friend: span (Span)]    => [friend]    { this.span = span }
	has [sealed: span (Span)]    => [sealed]    { this.span = span }
	has [unordered: span (Span)] => [unordered] { this.span = span }
	has [getter: span (Span)]    => [getter]    { this.span = span }
	has [setter: span (Span)]    => [setter]    { this.span = span }
	has [main: span (Span)]      => [main]      { this.span = span }
	has [inline: span (Span)]    => [inline]    { this.span = span }
	has [noinherit: span (Span)] => [noinherit] { this.span = span }
	has [pattern: span (Span)]   => [pattern]   { this.span = span }
	has [asm: span (Span)]       => [asm]       { this.span = span }
	has [native: span (Span)]    => [native]    { this.span = span }
	has [flags: span (Span)]     => [flags]     { this.span = span }
	has [uncounted: span (Span)] => [uncounted] { this.span = span }
	has [strong: span (Span)]    => [strong]    { this.span = span }
	has [sealed: span (Span)]    => [sealed]    { this.span = span }
	has [macro: span (Span)]     => [macro]     { this.span = span }

	has [tilde: span (Span)]        => [tilde]        { this.span = span }
	has [dot: span (Span)]          => [dot]          { this.span = span }
	has [eq: span (Span)]           => [eq]           { this.span = span }
	has [eqGt: span (Span)]         => [eqGt]         { this.span = span }
	has [plus: span (Span)]         => [plus]         { this.span = span }
	has [plusEq: span (Span)]       => [plusEq]       { this.span = span }
	has [plusPlus: span (Span)]     => [plusPlus]     { this.span = span }
	has [minus: span (Span)]        => [minus]        { this.span = span }
	has [minusEq: span (Span)]      => [minusEq]      { this.span = span }
	has [minusMinus: span (Span)]   => [minusMinus]   { this.span = span }
	has [star: span (Span)]         => [star]         { this.span = span }
	has [starEq: span (Span)]       => [starEq]       { this.span = span }
	has [starStar: span (Span)]     => [starStar]     { this.span = span }
	has [starStarEq: span (Span)]   => [starStarEq]   { this.span = span }
	has [div: span (Span)]          => [div]          { this.span = span }
	has [divEq: span (Span)]        => [divEq]        { this.span = span }
	has [divDiv: span (Span)]       => [divDiv]       { this.span = span }
	has [divDivEq: span (Span)]     => [divDivEq]     { this.span = span }
	has [mod: span (Span)]          => [mod]          { this.span = span }
	has [modEq: span (Span)]        => [modEq]        { this.span = span }
	has [modMod: span (Span)]       => [modMod]       { this.span = span }
	has [modModEq: span (Span)]     => [modModEq]     { this.span = span }
	has [and: span (Span)]          => [and]          { this.span = span }
	has [andEq: span (Span)]        => [andEq]        { this.span = span }
	has [andAnd: span (Span)]       => [andAnd]       { this.span = span }
	has [andAndEq: span (Span)]     => [andAndEq]     { this.span = span }
	has [bar: span (Span)]          => [bar]          { this.span = span }
	has [barEq: span (Span)]        => [barEq]        { this.span = span }
	has [barBar: span (Span)]       => [barBar]       { this.span = span }
	has [barBarEq: span (Span)]     => [barBarEq]     { this.span = span }
	has [caret: span (Span)]        => [caret]        { this.span = span }
	has [caretEq: span (Span)]      => [caretEq]      { this.span = span }
	has [caretCaret: span (Span)]   => [caretCaret]   { this.span = span }
	has [caretCaretEq: span (Span)] => [caretCaretEq] { this.span = span }
	has [bang: span (Span)]         => [bang]         { this.span = span }
	has [bangEq: span (Span)]       => [bangEq]       { this.span = span }
	has [bangBang: span (Span)]     => [bangBang]     { this.span = span }
	has [bangBangEq: span (Span)]   => [bangBangEq]   { this.span = span }
	has [question: span (Span)]     => [question]     { this.span = span }
	has [questionEq: span (Span)]   => [questionEq]   { this.span = span }
	has [gt: span (Span)]           => [gt]           { this.span = span }
	has [gtEq: span (Span)]         => [gtEq]         { this.span = span }
	has [gtGt: span (Span)]         => [gtGt]         { this.span = span }
	has [gtGtEq: span (Span)]       => [gtGtEq]       { this.span = span }
	has [lt: span (Span)]           => [lt]           { this.span = span }
	has [ltEq: span (Span)]         => [ltEq]         { this.span = span }
	has [ltLt: span (Span)]         => [ltLt]         { this.span = span }
	has [ltLtEq: span (Span)]       => [ltLtEq]       { this.span = span }
	has [dotDotDot: span (Span)]    => [dotDotDot]    { this.span = span }
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
	has [this: span (Span)]     => [this]     { this.span = span }
	has [wildcard: span (Span)] => [wildcard] { this.span = span }
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
			at Token[else] => return Token[:span name: "else"]
			at Token[while] => return Token[:span name: "while"]
			at Token[for] => return Token[:span name: "for"]
			at Token[recurse] => return Token[:span name: "recurse"]
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

	on [maybeAssignableOp] (Maybe[Tuple[Span, Maybe[Assignable]]]) {
		match this {
			at This[eq] => return Maybe[the: #{span, Maybe[Assignable][none]}]
			at This[plusEq] => return Maybe[the: #{span, Maybe[the: Assignable[plus]]}]
			at This[minusEq] => return Maybe[the: #{span, Maybe[the: Assignable[minus]]}]
			at This[starEq] => return Maybe[the: #{span, Maybe[the: Assignable[times]]}]
			at This[starStarEq] => return Maybe[the: #{span, Maybe[the: Assignable[pow]]}]
			at This[divEq] => return Maybe[the: #{span, Maybe[the: Assignable[div]]}]
			at This[divDivEq] => return Maybe[the: #{span, Maybe[the: Assignable[intDiv]]}]
			at This[modEq] => return Maybe[the: #{span, Maybe[the: Assignable[mod]]}]
			at This[modModEq] => return Maybe[the: #{span, Maybe[the: Assignable[isMod]]}]
			at This[andEq] => return Maybe[the: #{span, Maybe[the: Assignable[bitAnd]]}]
			at This[andAndEq] => return Maybe[the: #{span, Maybe[the: Assignable[and]]}]
			at This[barEq] => return Maybe[the: #{span, Maybe[the: Assignable[bitOr]]}]
			at This[barBarEq] => return Maybe[the: #{span, Maybe[the: Assignable[or]]}]
			at This[caretEq] => return Maybe[the: #{span, Maybe[the: Assignable[bitXor]]}]
			at This[caretCaretEq] => return Maybe[the: #{span, Maybe[the: Assignable[xor]]}]
			at This[bangBangEq] => return Maybe[the: #{span, Maybe[the: Assignable[nor]]}]
			at This[ltLtEq] => return Maybe[the: #{span, Maybe[the: Assignable[shl]]}]
			at This[gtGtEq] => return Maybe[the: #{span, Maybe[the: Assignable[shr]]}]
			else => return Maybe[none]
		}
	}
}