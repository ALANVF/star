package lexing;

import text.Span;

enum StrSegment {
	SStr(str: String);
	SChar(char: Char);
	SCode(tokens: Tokens);
}

@:using(Token.TokenHelper)
enum Token {
	T_CSep(_: Span);
	T_Comma(_: Span);
	T_LSep(_: Span);

	T_Module(_: Span);
	T_My(_: Span);
	T_On(_: Span);
	T_Return(_: Span);
	T_Init(_: Span);
	T_Deinit(_: Span);
	T_Operator(_: Span);
	T_Class(_: Span);
	T_Alias(_: Span);
	T_Type(_: Span);
	T_Kind(_: Span);
	T_Category(_: Span);
	T_Protocol(_: Span);
	T_Is(_: Span);
	T_Of(_: Span);
	T_Use(_: Span);
	T_Has(_: Span);
	T_If(_: Span);
	T_Else(_: Span);
	T_While(_: Span);
	T_For(_: Span);
	T_Do(_: Span);
	T_Case(_: Span);
	T_Match(_: Span);
	T_At(_: Span);
	T_Break(_: Span);
	T_Next(_: Span);
	T_Throw(_: Span);
	T_Try(_: Span);
	T_Catch(_: Span);

	T_Static(_: Span);
	T_Hidden(_: Span);
	T_Readonly(_: Span);
	T_Friend(_: Span);
	T_Unordered(_: Span);
	T_Getter(_: Span);
	T_Setter(_: Span);
	T_Main(_: Span);
	T_Inline(_: Span);
	T_Noinherit(_: Span);
	T_Pattern(_: Span);
	T_Asm(_: Span);
	T_Native(_: Span);
	T_Flags(_: Span);
	T_Uncounted(_: Span);
	T_Strong(_: Span);
	T_Sealed(_: Span);
	T_Macro(_: Span);

	T_Tilde(_: Span);
	T_Dot(_: Span);
	T_Eq(_: Span);
	T_EqGt(_: Span);
	T_Plus(_: Span);
	T_PlusEq(_: Span);
	T_PlusPlus(_: Span);
	T_Minus(_: Span);
	T_MinusEq(_: Span);
	T_MinusMinus(_: Span);
	T_Star(_: Span);
	T_StarEq(_: Span);
	T_StarStar(_: Span);
	T_StarStarEq(_: Span);
	T_Div(_: Span);
	T_DivEq(_: Span);
	T_DivDiv(_: Span);
	T_DivDivEq(_: Span);
	T_Mod(_: Span);
	T_ModEq(_: Span);
	T_ModMod(_: Span);
	T_ModModEq(_: Span);
	T_And(_: Span);
	T_AndEq(_: Span);
	T_AndAnd(_: Span);
	T_AndAndEq(_: Span);
	T_Bar(_: Span);
	T_BarEq(_: Span);
	T_BarBar(_: Span);
	T_BarBarEq(_: Span);
	T_Caret(_: Span);
	T_CaretEq(_: Span);
	T_CaretCaret(_: Span);
	T_CaretCaretEq(_: Span);
	T_Bang(_: Span);
	T_BangEq(_: Span);
	T_BangBang(_: Span);
	T_BangBangEq(_: Span);
	T_Question(_: Span);
	T_QuestionEq(_: Span);
	T_Gt(_: Span);
	T_GtEq(_: Span);
	T_GtGt(_: Span);
	T_GtGtEq(_: Span);
	T_Lt(_: Span);
	T_LtEq(_: Span);
	T_LtLt(_: Span);
	T_LtLtEq(_: Span);
	T_DotDotDot(_: Span);
	T_Cascade(_: Span, depth: Int);

	T_LParen(_: Span);
	T_LBracket(_: Span);
	T_LBrace(_: Span);
	T_HashLParen(_: Span);
	T_HashLBracket(_: Span);
	T_HashLBrace(_: Span);
	T_RParen(_: Span);
	T_RBracket(_: Span);
	T_RBrace(_: Span);

	T_Name(_: Span, name: String);
	T_TypeName(_: Span, name: String);
	T_Label(_: Span, label: String);
	T_Punned(_: Span, punned: String);
	T_Tag(_: Span, tag: String);
	T_Litsym(_: Span, litsym: String);

	T_Int(_: Span, int: String, exp: Option<String>);
	T_Hex(_: Span, hex: String);
	T_Dec(_: Span, int: String, dec: String, exp: Option<String>);
	T_Str(_: Span, segments: List<StrSegment>);
	T_Char(_: Span, char: Char);
	T_Bool(_: Span, bool: Bool);
	T_This(_: Span);
	T_Wildcard(_: Span);
	T_AnonArg(_: Span, depth: Int, nth: Int);
}


@:noCompletion
@:publicFields
class TokenHelper {
	static function span(token: Token): Span {
		return token.getParameters()[0];
	}

	static function basicTokenName(token: Token) return token._match(
		at(T_CSep(_) | T_Comma(_)) => "comma",
		at(T_LSep(_)) => "newline",
		at(T_Module(_) ... T_Catch(_)) => "keyword",
		at(T_Static(_) ... T_Macro(_)) => "attribute",
		at(T_Dot(_)) => "dot",
		at(T_Tilde(_) | (T_Eq(_) ... T_DotDotDot(_))) => "operator",
		at(T_Cascade(_, _)) => "cascade",
		at(T_LParen(_)) => "opening parenthesis",
		at(T_LBracket(_)) => "opening bracket",
		at(T_LBrace(_)) => "opening brace",
		at(T_HashLParen(_)) => "hash and opening parenthesis",
		at(T_HashLBracket(_)) => "hash and opening bracket",
		at(T_HashLBrace(_)) => "hash and opening brace",
		at(T_RParen(_)) => "closing parenthesis",
		at(T_RBracket(_)) => "closing bracket",
		at(T_RBrace(_)) => "closing brace",
		at(T_Name(_, _)) => "name",
		at(T_TypeName(_, _)) => "type name",
		at(T_Label(_, _)) => "label",
		at(T_Punned(_, _)) => "punned label",
		at(T_Tag(_, _)) => "tag",
		at(T_Litsym(_, _)) => "litsym",
		at(T_Int(_, _, _)) => "integer literal",
		at(T_Hex(_, _)) => "hexdecimal literal",
		at(T_Dec(_, _, _, _)) => "decimal literal",
		at(T_Str(_, _)) => "string literal",
		at(T_Char(_, _)) => "character literal",
		at(T_Bool(_, _)) => "boolean literal",
		at(T_This(_)) => "`this` literal",
		at(T_Wildcard(_)) => "wildcard literal",
		at(T_AnonArg(_, _, _)) => "anonymous argument literal"
	);

	static function asSoftName(token: Token) return switch token {
		case T_Module(span): T_Name(span, "module");
		case T_On(span): T_Name(span, "on");
		case T_Init(span): T_Name(span, "init");
		case T_Deinit(span): T_Name(span, "deinit");
		case T_Operator(span): T_Name(span, "operator");
		case T_Class(span): T_Name(span, "class");
		case T_Alias(span): T_Name(span, "alias");
		case T_Type(span): T_Name(span, "type");
		case T_Kind(span): T_Name(span, "kind");
		case T_Category(span): T_Name(span, "category");
		case T_Protocol(span): T_Name(span, "protocol");
		//T_Is
		case T_Of(span): T_Name(span, "of");
		case T_Use(span): T_Name(span, "use");
		//T_Has
		default: token;
	}

	static function asAnyName(token: Token) return switch token {
		case T_Module(span): T_Name(span, "module");
		case T_My(span): T_Name(span, "my");
		case T_On(span): T_Name(span, "on");
		case T_Return(span): T_Name(span, "return");
		case T_Init(span): T_Name(span, "init");
		case T_Deinit(span): T_Name(span, "deinit");
		case T_Operator(span): T_Name(span, "operator");
		case T_Class(span): T_Name(span, "class");
		case T_Alias(span): T_Name(span, "alias");
		case T_Type(span): T_Name(span, "type");
		case T_Kind(span): T_Name(span, "kind");
		case T_Category(span): T_Name(span, "category");
		case T_Protocol(span): T_Name(span, "protocol");
		case T_Is(span): T_Name(span, "is");
		case T_Of(span): T_Name(span, "of");
		case T_Use(span): T_Name(span, "use");
		case T_Has(span): T_Name(span, "has");
		case T_If(span): T_Name(span, "if");
		case T_Else(span): T_Name(span, "else");
		case T_While(span): T_Name(span, "while");
		case T_For(span): T_Name(span, "for");
		case T_Do(span): T_Name(span, "do");
		case T_Case(span): T_Name(span, "case");
		case T_Match(span): T_Name(span, "match");
		case T_At(span): T_Name(span, "at");
		case T_Break(span): T_Name(span, "break");
		case T_Next(span): T_Name(span, "next");
		case T_Throw(span): T_Name(span, "throw");
		case T_Try(span): T_Name(span, "try");
		case T_Catch(span): T_Name(span, "catch");
		case T_Bool(span, true): T_Name(span, "true");
		case T_Bool(span, false): T_Name(span, "false");
		case T_This(span): T_Name(span, "this");
		default: token;
	}
}