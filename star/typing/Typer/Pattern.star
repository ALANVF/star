;[ SPECS

pattern ::=
	| <expr>
	| <extractor>                // (conditional) expr with "_" as a placeholder for the target value
	| "_"
	| "my" <ident> <type>?
	| "(" <type> ")"
	| <pattern> "=" <pattern>
	| <pattern> "&&" <pattern>
	| <pattern> "||" <pattern>
	| <pattern> "^^" <pattern>
	| <pattern> "!!" <pattern>
	| "!" <pattern>
	| <memberwise-constructor>
	| <variant-case>             // optionally with memberwise constructor
	| <multi-variant-case>
	| <multi-variant-case> "&" <pattern>
	| <pattern> "^" <multi-variant-case>
	| "~" <multi-variant-case>
	| <type>? "#[" <array-pattern>* "]"
	| <type>? "#{" <pattern>* "}"


array-pattern ::=
	| "..." <pattern>
	| <pattern> "?"
	| <pattern>


5
1 <= _ < 10
_[method]
_
my foo
my foo (Bar)
(Type)
my foo = pattern
#{a, my b} = _[method]
#[_, foo] && _ != bar
1 || 2
#{1, _, _} ^^ #{_, 2, _} ^^ #{_, _, 3}
#"a" !! #"b" !! _ > #"z"
!#[1, 2]
Point[x: my x, y: 5]
Maybe[the: my value]
Token[name: _, span: my span]
Attr[color: "red"]
Attr[color: "red"] & Attr[bold]
my otherAttrs & Attr[color: "red"]
_ ^ Attr[color: "red"]
~Options.flag1
#[1, 2?, ...my rest]
Tokens #[Token[lSep], ...my rest]
#{1, 2.3}
Point #{my x, 5}

]

kind Bound {
	has inclusive
	has exclusive
}

kind Pattern {
	;-- A normal expression
	has [expr: (Expr)]

	;-- An "extractor"; `_` represents the value being matched on, returning an expression
	has [extractor: (Expr)]
	has [extractMessage: (ObjMessage)]

	;-- Wildcard
	has [ignore]

	;-- A capture
	has [my: name (Str)]

	;-- A typed capture
	has [my: name (Str) type: (Type)]

	;-- A type check
	has [type: (Type)]

	;-- "and" pattern
	has [all: patterns (Array[Pattern])]

	;-- "or" pattern
	has [any: patterns (Array[Pattern])]
	
	;-- "xor" pattern
	has [one: patterns (Array[Pattern])]

	;-- "nor" pattern
	has [none: patterns (Array[Pattern])]

	;-- "not" pattern
	has [not: (Pattern)]

	;-- Range pattern
	has [min: (Pattern), (Bound) pattern: (Pattern)]
	has [pattern: (Pattern) max: (Pattern), (Bound)]
	has [min: (Pattern), (Bound) pattern: (Pattern) max: (Pattern), (Bound)]

	;-- Destructuring pattern
	has [assign: (Pattern) pattern: (Pattern)]

	;-- Array pattern
	has [array: patterns (Array[ArrayPattern])]
	has [type: (Type) array: patterns (Array[ArrayPattern])]

	;@@ TODO: hash pattern

	;-- Tuple pattern
	has [tuple: patterns (Array[Pattern])]
	has [type: (Type) tuple: patterns (Array[Pattern])]

	;-- Memberwise pattern
	has [type: (Type) members: (Array[Tuple[Member, Pattern]])]

	;-- Value kind case
	has [type: (Type) valueCase: (ValueCase)]

	;-- Tagged kind case
	has [type: (Type) taggedCase: (TaggedCase.Single)]
	has [type: (Type) taggedCase: (TaggedCase.Multi), (Array[Pattern])]

	;-- Tagged kind case w/ memberwise pattern
	has [type: (Type) taggedCase: (TaggedCase.Single) members: (Array[Tuple[Member, Pattern]])]
	has [type: (Type) taggedCase: (TaggedCase.Multi), args (Array[Pattern]) members: (Array[Tuple[Member, Pattern]])]

	;-- `&` mulit-kind pattern
	has [extract: (Pattern) from: (Pattern)]

	;-- `^` multi-kind pattern
	has [exclude: (Pattern) from: (Pattern)]

	;-- `~` multi-kind pattern
	has [complement: (Pattern)]

	
	my t (Maybe[Type]) = Maybe[none]
	my orig (Maybe[Parser.Expr]) = Maybe[none]
}

kind ArrayPattern of Pattern {
	;-- "spread" pattern
	has [spread: (Pattern)]

	;-- Optional pattern
	has [optional: (Pattern)]
}