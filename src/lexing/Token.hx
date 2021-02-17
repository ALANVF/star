package lexing;

import text.Span;

enum StrSegment {
	SStr(str: String);
	SChar(char: Char);
	SCode(tokens: List<Token>);
}

@:using(Token.TokenHelper)
enum Token {
	T_CSep(_: Span);
	T_Comma(_: Span);
	T_LSep(_: Span);

	T_Module(_: Span);
	T_Macro(_: Span);
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
	T_Orif(_: Span);
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
	T_Str(_: Span, segments: Array<StrSegment>);
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

	static function basicTokenName(token: Token) return switch token {
		case T_CSep(_) | T_Comma(_): "comma";
		case T_LSep(_): "newline";
		case T_Module(_) | T_Macro(_) | T_My(_) | T_On(_) | T_Return(_)
			| T_Init(_) | T_Deinit(_) | T_Operator(_) | T_Class(_) | T_Alias(_)
			| T_Type(_) | T_Kind(_) | T_Category(_) | T_Protocol(_) | T_Is(_)
			| T_Of(_) | T_Use(_) | T_Has(_) | T_If(_) | T_Orif(_) | T_Else(_)
			| T_While(_) | T_For(_) | T_Do(_) | T_Case(_) | T_Match(_) | T_At(_)
			| T_Break(_) | T_Next(_) | T_Throw(_) | T_Try(_) | T_Catch(_): "keyword";
		case T_Static(_) | T_Hidden(_) | T_Readonly(_) | T_Friend(_) | T_Unordered(_)
			| T_Getter(_) | T_Setter(_) | T_Main(_) | T_Inline(_) | T_Noinherit(_)
			| T_Pattern(_) | T_Asm(_) | T_Native(_) | T_Flags(_) | T_Uncounted(_)
			| T_Strong(_) | T_Sealed(_): "attribute";
		case T_Dot(_): "dot";
		case T_Tilde(_) | T_Eq(_) | T_EqGt(_)
			| T_Plus(_) | T_PlusEq(_) | T_PlusPlus(_)
			| T_Minus(_) | T_MinusEq(_) | T_MinusMinus(_)
			| T_Star(_) | T_StarEq(_) | T_StarStar(_) | T_StarStarEq(_)
			| T_Div(_) | T_DivEq(_) | T_DivDiv(_) | T_DivDivEq(_)
			| T_Mod(_) | T_ModEq(_) | T_ModMod(_) | T_ModModEq(_)
			| T_And(_) | T_AndEq(_) | T_AndAnd(_) | T_AndAndEq(_)
			| T_Bar(_) | T_BarEq(_) | T_BarBar(_) | T_BarBarEq(_)
			| T_Caret(_) | T_CaretEq(_) | T_CaretCaret(_) | T_CaretCaretEq(_)
			| T_Bang(_) | T_BangEq(_) | T_BangBang(_) | T_BangBangEq(_)
			| T_Question(_) | T_QuestionEq(_)
			| T_Gt(_) | T_GtEq(_) | T_GtGt(_) | T_GtGtEq(_)
			| T_Lt(_) | T_LtEq(_) | T_LtLt(_) | T_LtLtEq(_)
			| T_DotDotDot(_): "operator";
		case T_Cascade(_, _): "cascade";
		case T_LParen(_): "opening parenthesis";
		case T_LBracket(_): "opening bracket";
		case T_LBrace(_): "opening brace";
		case T_HashLParen(_): "hash and opening parenthesis";
		case T_HashLBracket(_): "hash and opening bracket";
		case T_HashLBrace(_): "hash and opening brace";
		case T_RParen(_): "closing parenthesis";
		case T_RBracket(_): "closing bracket";
		case T_RBrace(_): "closing brace";
		case T_Name(_, _): "name";
		case T_TypeName(_, _): "type name";
		case T_Label(_, _): "label";
		case T_Punned(_, _): "punned label";
		case T_Tag(_, _): "tag";
		case T_Litsym(_, _): "litsym";
		case T_Int(_, _, _): "integer literal";
		case T_Hex(_, _): "hexdecimal literal";
		case T_Dec(_, _, _, _): "decimal literal";
		case T_Str(_, _): "string literal";
		case T_Char(_, _): "character literal";
		case T_Bool(_, _): "boolean literal";
		case T_This(_): "`this` literal";
		case T_Wildcard(_): "wildcard literal";
		case T_AnonArg(_, _, _): "anonymous argument literal";
	}

	static function asSoftName(token: Token) return switch token {
		case T_Module(span): T_Name(span, "module");
		case T_Macro(span): T_Name(span, "macro");
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
		case T_Macro(span): T_Name(span, "macro");
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
		case T_Orif(span): T_Name(span, "orif");
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