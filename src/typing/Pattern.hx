package typing;

/* SPECS

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

*/

enum Bound {
	Inclusive;
	Exclusive;
}

enum PatternKind {
	// A normal expression
	PExpr(expr: TExpr);

	// An "extractor"; `_` represents the value being matched on, returning an expression
	PExtractor(extractor: TExpr);
	PExtractMessage(msg: ObjMessage);

	// Wildcard
	PIgnore;

	// A capture
	PMy(name: String);

	// A typed capture
	PMyType(name: String, type: Type);

	// A type check
	PType(type: Type);

	// "and" pattern
	PAll(patterns: Array<Pattern>);

	// "or" pattern
	PAny(patterns: Array<Pattern>);
	
	// "xor" pattern
	POne(patterns: Array<Pattern>);

	// "nor" pattern
	PNone(patterns: Array<Pattern>);

	// "not" pattern
	PNot(not: Pattern);

	// Range pattern
	PBoundsMin(min: Pattern, _: Bound, pattern: Pattern);
	PBoundsMax(pattern: Pattern, max: Pattern, _: Bound);
	PBoundsMinMax(min: Pattern, _1: Bound, pattern: Pattern, max: Pattern, _2: Bound);

	// Destructuring pattern
	PAssignPattern(assign: Pattern, pattern: Pattern);

	// Array pattern
	PArray(patterns: Array</*Array*/Pattern>);
	PTypeArray(type: Type, patterns: Array</*Array*/Pattern>);

	// TODO: hash pattern

	// Tuple pattern
	PTuple(patterns: Array<Pattern>);
	PTypeTuple(type: Type, patterns: Array<Pattern>);

	// Memberwise pattern
	PTypeMembers(type: Type, members: Array<Tuple2<Member, Pattern>>);

	// Value kind case
	PTypeValueCase(type: Type, valueCase: ValueCase);

	// Tagged kind case
	PTypeTaggedCaseSingle(type: Type, taggedCase: SingleTaggedCase);
	PTypeTaggedCaseMulti(type: Type, taggedCase: MultiTaggedCase, _: Array<Pattern>);

	// Tagged kind case w/ memberwise pattern
	PTypeTaggedCaseMembersSingle(type: Type, taggedCase: SingleTaggedCase, members: Array<Tuple2<Member, Pattern>>);
	PTypeTaggedCaseMembersMulti(type: Type, taggedCase: MultiTaggedCase, args: Array<Pattern>, members: Array<Tuple2<Member, Pattern>>);

	// `&` mulit-kind pattern
	PExtractFrom(extract: Pattern, from: Pattern);

	// `^` multi-kind pattern
	PExcludeFrom(exclude: Pattern, from: Pattern);

	// `~` multi-kind pattern
	PComplement(complement: Pattern);

	// ==== ARRAY PATTERNS =====

	// "spread" pattern
	PSpread(spread: Pattern);

	// Optional pattern
	POptional(optional: Pattern);
}

@:publicFields @:structInit class Pattern {
	final p: PatternKind;
	var t: Null<Type> = null;
	var orig: Null<parsing.ast.Expr> = null;

	function isMultiKind() return p._match(
		at(PTypeValueCase(type, _)
			| PTypeTaggedCaseSingle(type, _)
			| PTypeTaggedCaseMulti(type, _, _)) => type.isFlags(),
		at(PExtractFrom(_, _) | PExcludeFrom(_, _) | PComplement(_)) => true,
		_ => false
	);

	@:keep function toString() {
return 'Pattern{
	p: $p,
	t: $t,
	orig: ${orig.mainSpan().display()}
}';
	}
}