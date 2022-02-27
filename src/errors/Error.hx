package errors;

import reporting.*;
import text.Span;

import lexing.Token;
import parsing.ast.decls.Operator as UOperator;
import parsing.ast.decls.Decl as UDecl;
import typing.AnyTypeDecl;
import typing.IDecl;
import typing.File;
import typing.Ctx;
import typing.AnyMethod;
import typing.MultiTaggedCase;
import typing.TaggedKind;
import typing.Type;
import typing.Category;
import typing.TExpr;


@:using(errors.Error)
enum Error {
	// Lexing errors

	Lex_UnterminatedComment(
		_begin: Span
	);
	
	Lex_InvalidOperator(
		name: String,
		_name: Span
	);

	Lex_InvalidInputAfterHash(
		_start: Span,
		next: String,
		_next: Span
	);

	Lex_InvalidEqEq(
		_1: Span
	);

	Lex_UnterminatedCascade(
		_begin: Span,
		_end: Span
	);

	Lex_InvalidInput(
		_1: Span
	);

	Lex_InvalidHexStart(
		_1: Span
	);

	Lex_NameAfterHex(
		_hex: Span,
		_name: Span
	);

	Lex_IncompleteDecimalPoint(
		_int: Span,
		_point: Span
	);

	Lex_NameAfterNumber(
		_num: Span,
		_name: Span
	);

	Lex_MissingExponent(
		_e: Span,
		_exp: Span
	);

	Lex_NoUppercasePunnedLabel(
		_start: Span,
		_head: Span,
		_rest: Span
	);

	Lex_IncompletePunnedLabel(
		_start: Span,
		_name: Span
	);

	Lex_NoUppercaseLabel(
		_head: Span,
		_rest: Span
	);

	Lex_EscapeCharQuote(
		_begin: Span,
		_quote: Span,
		_end: Span
	);

	Lex_NoEmptyChar(
		_char: Span
	);

	Lex_InvalidCharEscape(
		_begin: Span,
		char: Char,
		_char: Span,
		_end: Span
	);

	Lex_UnterminatedChar(
		_begin: Span,
		_end: Span
	);

	Lex_InvalidHexEscape(
		_start: Span,
		_esc: Span
	);

	Lex_InvalidUniEscape(
		_start: Span,
		_esc: Span
	);

	Lex_InvalidOctEscape(
		_start: Span,
		_esc: Span
	);

	Lex_InvalidStrEscape(
		char: Char,
		_char: Span
	);

	Lex_UnterminatedStr(
		_begin: Span
	);

	Lex_NameAfterAnonArg(
		_arg: Span,
		_name: Span
	);

	Lex_UnterminatedAnonArg(
		_begin: Span,
		_end: Span
	);


	// Parsing errors

	Parse_UnexpectedTokenWantedSep(
		token: Token
	);

	Parse_UnexpectedToken(
		first: Token,
		?last: Token
	);

	Parse_UnexpectedEOF(
		first: Token,
		last: Token
	);

	Parse_NoGenericMember(
		_1: Span
	);

	Parse_NoGenericCase(
		_1: Span
	);

	Parse_NoGenericDeinit(
		_1: Span
	);


	// Typing errors

	TooManyErrors;

	Type_UnorganizedCode(
		_1: Span
	);

	Type_UnknownPragma(
		pragma: String,
		_pragma: Span
	);

	Type_RedundantGetter(
		member: String,
		_member: Span,
		getter: String,
		_getter: Span
	);

	Type_RedundantSetter(
		member: String,
		_member: Span,
		setter: String,
		_setter: Span
	);

	Type_RedundantGetterSetter(
		member: String,
		_member: Span,
		_getter: Span,
		_setter: Span
	);

	Type_OpNotOverloadable(
		decl: AnyTypeDecl,
		op: UOperator,
		yet: Bool
	);

	Type_OpNeedsParameter(
		decl: AnyTypeDecl,
		op: UOperator
	);

	Type_OpDoesNotNeedParameter(
		decl: AnyTypeDecl,
		op: UOperator
	);

	Type_UnknownOpOverload(
		decl: AnyTypeDecl,
		op: UOperator
	);

	Type_NoTaggedKindRepr(
		kind: TaggedKind,
		_repr: Span
	);

	Type_NoValueCaseInit(
		vcase: String,
		_vcase: Span,
		_init: Span
	);

	Type_DuplicateAttribute(
		decl: IDecl,
		name: String,
		attr: String,
		_attr: Span
	);
	Type_InvalidAttribute(
		decl: IDecl,
		name: String,
		attr: String,
		_attr: Span
	);

	Type_DuplicateDecl(
		decl: AnyTypeDecl,
		decl2: UDecl
	);
	Type_DuplicateDeclInFile(
		file: File,
		decl: UDecl
	);

	Type_UnexpectedDecl(
		decl: AnyTypeDecl,
		decl2: UDecl
	);
	Type_UnexpectedDeclInFile(
		file: File,
		decl: UDecl
	);

	Type_InvalidDecl(
		decl: AnyTypeDecl,
		decl2: UDecl
	);
	Type_InvalidDeclInFile(
		file: File,
		decl: UDecl
	);

	Type_InvalidTypeLookup(
		span: Span,
		?why: String
	);

	Type_InvalidTypeApply(
		span: Span,
		?why: String
	);

	Type_NotYetImplemented(
		span: Span,
		?why: String
	);

	Type_DuplicateParam(
		mth: AnyMethod,
		name: String,
		origSpan: Span,
		dupSpan: Span
	);
	Type_DuplicateCaseParam(
		tcase: MultiTaggedCase,
		name: String,
		origSpan: Span,
		dupSpan: Span
	);

	Type_UnknownFieldOrVar(
		ctx: Ctx,
		name: String,
		_name: Span
	);

	Type_ShadowedLocalVar(
		ctx: Ctx,
		name: String,
		origSpan: Span,
		dupSpan: Span
	);

	Type_LocalVarTypeMismatch(
		ctx: Ctx,
		name: String,
		wantedType: Type,
		gotType: Type,
		declSpan: Span,
		hereSpan: Span
	);

	Type_UnknownMethod(
		ctx: Ctx,
		type: Type,
		kind: MethodKind,
		span: Span,
		?categories: Array<Category>
	);

	Type_UnknownCast(
		ctx: Ctx,
		type: Type,
		target: Type,
		span: Span,
		?categories: Array<Category>
	);

	Type_UnknownGetter(
		ctx: Ctx,
		access: Access,
		type: Type,
		name: String,
		span: Span
	);

	Type_UnknownSetter(
		ctx: Ctx,
		access: Access,
		type: Type,
		name: String,
		span: Span,
		?value: TExpr
	);

	Type_UnknownCategory(
		ctx: Ctx,
		access: Access,
		type: Type,
		cat: Type,
		span: Span
	);

	Type_ThisNotAllowed(
		ctx: Ctx,
		span: Span
	);

	Type_ExpectedLogicalValue(
		ctx: Ctx,
		gotType: Type,
		span: Span
	);

	Type_PossiblyUnintendedArrowBlock(
		ctx: Ctx,
		span: Span
	);

	Type_ArrayPatternNotAllowed(
		ctx: Ctx,
		span: Span
	);

	Type_DuplicateBinding(
		ctx: Ctx,
		name: String,
		origSpan: Span,
		dupSpan: Span
	);
}


function asDiagnostic(self: Error) { return new Diagnostic(self._match(
	// Lexing errors

	at(Lex_UnterminatedComment(_begin)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: _begin,
				message: "Unterminated comment",
				isPrimary: true
			})
		]
	},

	at(Lex_InvalidOperator(name, _name)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: _name,
				message: 'Invalid operator `$name`',
				isPrimary: true
			}),
		]
	},

	at(Lex_InvalidInputAfterHash(_start, next, _next)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: _next,
				message: 'Unexpected `$next` after `#`',
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidEqEq(_1)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: _1,
				message: "Please use `?=` instead of `==` in Star",
				isPrimary: true
			})
		]
	},

	at(Lex_UnterminatedCascade(_begin, _end)) => {
		severity: Severity.ERROR,
		message: "Unterminated cascade",
		info: [
			Spanned({
				span: _end,
				message: "Expected a `>` to finish the cascade operator",
				isPrimary: true
			}),
			Spanned({
				span: _begin,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidInput(_1)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: _1,
				message: "This is not the syntax that you are looking for",
				isPrimary: true
			})
		]
	},

	at(Lex_InvalidHexStart(_1)) => {
		severity: Severity.ERROR,
		message: "Unexpected start of hexdecimal literal",
		info: [
			Spanned({
				span: _1,
				message: "Were you wanting a hexdecimal literal here or what?",
				isPrimary: true
			})
		]
	},

	at(Lex_NameAfterHex(_hex, _name)) => {
		severity: Severity.ERROR,
		message: "Invalid hexdecimal literal",
		info: [
			Spanned({
				span: _name,
				message: "Make sure to separate names from numbers",
				isPrimary: true
			}),
			Spanned({
				span: _hex,
				isSecondary: true
			})
		]
	},

	at(Lex_IncompleteDecimalPoint(_int, _point)) => {
		severity: Severity.ERROR,
		message: "Invalid decimal literal",
		info: [
			Spanned({
				span: _point,
				message: "At least 1 digit is required on both sides of the decimal point",
				isPrimary: true
			}),
			Spanned({
				span: _int,
				isSecondary: true
			})
		]
	},

	at(Lex_NameAfterNumber(_num, _name)) => {
		severity: Severity.ERROR,
		message: "Invalid number literal",
		info: [
			Spanned({
				span: _name,
				message: "Make sure to separate names from numbers",
				isPrimary: true
			}),
			Spanned({
				span: _num,
				isSecondary: true
			})
		]
	},

	at(Lex_MissingExponent(_e, _exp)) => {
		severity: Severity.ERROR,
		message: "Invalid number literal",
		info: [
			Spanned({
				span: _exp,
				message: "Expected a number after the exponent indicator",
				isPrimary: true
			}),
			Spanned({
				span: _e,
				message: "This indicates that the number has an exponent",
				isSecondary: true
			})
		]
	},

	at(Lex_NoUppercasePunnedLabel(_start, _head, _rest)) => {
		severity: Severity.ERROR,
		message: "Invalid punned label",
		info: [
			Spanned({
				span: _head,
				message: "Punned labels may not start with an uppercase letter",
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			}),
			Spanned({
				span: _rest,
				isSecondary: true
			})
		]
	},

	at(Lex_IncompletePunnedLabel(_start, _name)) => {
		severity: Severity.ERROR,
		message: "Invalid punned label",
		info: [
			Spanned({
				span: _name,
				message: "Was expecting a name for the punned label",
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			})
		]
	},

	at(Lex_NoUppercaseLabel(_head, _rest)) => {
		severity: Severity.ERROR,
		message: "Invalid label",
		info: [
			Spanned({
				span: _head,
				message: "Labels may not start with an uppercase letter",
				isPrimary: true
			}),
			Spanned({
				span: _rest,
				isSecondary: true
			})
		]
	},

	at(Lex_EscapeCharQuote(_begin, _quote, _end)) => {
		severity: Severity.ERROR,
		message: "Invalid char literal",
		info: [
			Spanned({
				span: _quote,
				message: "`\"` characters need to be escaped in char literals",
				isPrimary: true
			}),
			Spanned({
				span: _begin,
				isSecondary: true
			}),
			Spanned({
				span: _end,
				isSecondary: true
			})
		]
	},

	at(Lex_NoEmptyChar(_char)) => {
		severity: Severity.ERROR,
		message: "Invalid char literal",
		info: [
			Spanned({
				span: _char,
				message: "Char literals may not be empty",
				isPrimary: true
			})
		]
	},

	at(Lex_InvalidCharEscape(_begin, char, _char, _end)) => {
		severity: Severity.ERROR,
		message: "Invalid escape character",
		info: [
			// off by 1 errors?
			Spanned({
				span: _char,
				message: 'Escape character `$char` ' + (
					if(char == '('.code) "is not allowed in char literals"
					else "does not exist"
				),
				isPrimary: true
			}),
			Spanned({
				span: _begin,
				isSecondary: true
			}),
			Spanned({
				span: _end,
				isSecondary: true
			})
		]
	},

	at(Lex_UnterminatedChar(_begin, _end)) => {
		severity: Severity.ERROR,
		message: "Unterminated char literal",
		info: [
			Spanned({
				span: _end,
				message: "Expected another `\"` to finish the char literal",
				isPrimary: true
			}),
			Spanned({
				span: _begin,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidHexEscape(_start, _esc)) => {
		severity: Severity.ERROR,
		message: "Invalid hexdecimal escape code",
		info: [
			Spanned({
				span: _esc,
				message: "Was expecting a hexdecimal digit here",
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidUniEscape(_start, _esc)) => {
		severity: Severity.ERROR,
		message: "Invalid unicode escape code",
		info: [
			Spanned({
				span: _esc,
				message: "Was expecting a hexdecimal digit here",
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidOctEscape(_start, _esc)) => {
		severity: Severity.ERROR,
		message: "Invalid octal escape code",
		info: [
			Spanned({
				span: _esc,
				message: "Was expecting an octal digit here",
				isPrimary: true
			}),
			Spanned({
				span: _start,
				isSecondary: true
			})
		]
	},

	at(Lex_InvalidStrEscape(char, _char)) => {
		severity: Severity.ERROR,
		message: "Invalid escape character",
		info: [
			Spanned({
				span: _char,
				message: 'Escape character `\\$char` does not exist',
				isPrimary: true
			})
		]
	},

	at(Lex_UnterminatedStr(_begin)) => {
		severity: Severity.ERROR,
		message: "Unterminated string",
		info: [
			Spanned({
				span: _begin,
				message: "This string is never terminated",
				isPrimary: true
			})
		]
	},

	at(Lex_NameAfterAnonArg(_arg, _name)) => {
		severity: Severity.ERROR,
		message: "Invalid anonymous argument",
		info: [
			Spanned({
				span: _name,
				message: "Make sure to separate names from numbers",
				isPrimary: true
			}),
			Spanned({
				span: _arg,
				isSecondary: true
			})
		]
	},

	at(Lex_UnterminatedAnonArg(_begin, _end)) => {
		severity: Severity.ERROR,
		message: "Unterminated anonymous argument",
		info: [
			Spanned({
				span: _end,
				message: "Was expecting a number here",
				isPrimary: true
			}),
			Spanned({
				span: _begin,
				isSecondary: true
			})
		]
	},


	// Parsing errors

	at(Parse_UnexpectedTokenWantedSep(token)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: token.span(),
				message: 'Unexpected ${token.basicTokenName()}, was expecting a comma or newline instead',
				isPrimary: true
			})
		]
	},

	at(Parse_UnexpectedToken(first, last), when(last == null || first == last)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: first.span(),
				message: 'Unexpected ${first.basicTokenName()}',
				isPrimary: true
			})
		]
	},
	at(Parse_UnexpectedToken(first, last!!)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: last.span(),
				message: 'Unexpected ${last.basicTokenName()}',
				isPrimary: true
			}),
			Spanned({
				span: first.span(),
				message: "Starting here",
				isSecondary: true
			})
		]
	},

	at(Parse_UnexpectedEOF(first, last), when(first == last)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: last.span(),
				message: 'Unexpected end of file after ${last.basicTokenName()}',
				isPrimary: true
			})
		]
	},
	at(Parse_UnexpectedEOF(first, last)) => {
		severity: Severity.ERROR,
		message: "Syntax error",
		info: [
			Spanned({
				span: last.span(),
				message: 'Unexpected end of file after ${last.basicTokenName()}',
				isPrimary: true
			}),
			Spanned({
				span: first.span(),
				message: "Starting here",
				isSecondary: true
			})
		]
	},

	at(Parse_NoGenericMember(_1)) => {
		severity: Severity.ERROR,
		message: "Invalid member",
		info: [
			Spanned({
				span: _1,
				message: "Members are not allowed to be generic",
				isPrimary: true
			})
		]
	},

	at(Parse_NoGenericCase(_1)) => {
		severity: Severity.ERROR,
		message: "Invalid case",
		info: [
			Spanned({
				span: _1,
				message: "Cases are not allowed to be generic",
				isPrimary: true
			})
		]
	},

	at(Parse_NoGenericDeinit(_1)) => {
		severity: Severity.ERROR,
		message: "Invalid deinitializer",
		info: [
			Spanned({
				span: _1,
				message: "Deinitializers are not allowed to be generic",
				isPrimary: true
			})
		]
	},


	// Typing errors

	at(TooManyErrors) => {
		severity: Severity.ERROR,
		message: "Too many errors!",
		info: []
	},

	at(Type_UnorganizedCode(_1)) => {
		severity: Severity.WARNING,
		message: "Unorganized code",
		info: [
			Spanned({
				span: _1,
				message: "All imports should be at the beginning of the file",
				isSecondary: true
			})
		]
	},

	at(Type_UnknownPragma(pragma, _pragma)) => {
		severity: Severity.ERROR,
		message: "Unknown pragma",
		info: [
			Spanned({
				span: _pragma,
				message: 'Unknown pragma `$pragma`',
				isPrimary: true
			})
		]
	},

	at(Type_RedundantGetter(member, _member, getter, _getter)) => {
		severity: Severity.WARNING,
		message: "Redundant code",
		info: [
			Spanned({
				span: _getter,
				message: 'Unnecessary use of "is getter `$getter`". Doing "is getter" is just fine',
				isPrimary: true
			}),
			Spanned({
				span: _member,
				message: 'For member `$member`',
				isSecondary: true
			})
		]
	},

	at(Type_RedundantSetter(member, _member, setter, _setter)) => {
		severity: Severity.WARNING,
		message: "Redundant code",
		info: [
			Spanned({
				span: _setter,
				message: 'Unnecessary use of "is setter `$setter`". Doing "is setter" is just fine',
				isPrimary: true
			}),
			Spanned({
				span: _member,
				message: 'For member `$member`',
				isSecondary: true
			})
		]
	},

	at(Type_RedundantGetterSetter(member, _member, _getter, _setter)) => {
		severity: Severity.WARNING,
		message: "Redundant code",
		info: [
			Spanned({
				span: _getter,
				message: 'Unnecessary use of "is getter" along with "is setter"',
				isPrimary: true
			}),
			Spanned({
				span: _setter,
				isPrimary: true
			}),
			Spanned({
				span: _member,
				message: 'For member `$member`',
				isSecondary: true
			})
		]
	},

	at(Type_OpNotOverloadable(decl, op, yet)) => {
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: op.symbolSpan,
				message: 'The `${op.symbol}` operator cannot be overloaded' + (yet? " (yet)" : ""),
				isPrimary: true
			}),
			Spanned({
				span: op.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.fullName()}`',
				isSecondary: true
			})
		]
	},
	
	at(Type_OpNeedsParameter(decl, op)) => {
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: op.symbolSpan,
				message: 'Overloading the `${op.symbol}` operator requires a parameter',
				isPrimary: true
			}),
			Spanned({
				span: op.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.fullName()}`',
				isSecondary: true
			})
		]
	},

	at(Type_OpDoesNotNeedParameter(decl, op)) => {
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: op.symbolSpan,
				message: 'Overloading the `${op.symbol}` operator should not require a parameter',
				isPrimary: true
			}),
			Spanned({
				span: op.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.fullName()}`',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownOpOverload(decl, op)) => {
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: op.symbolSpan,
				message: 'The `${op.symbol}` operator cannot be overloaded because it does not exist',
				isPrimary: true
			}),
			Spanned({
				span: op.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.fullName()}`',
				isSecondary: true
			})
		]
	},

	at(Type_NoTaggedKindRepr(kind, _repr)) => {
		severity: Severity.ERROR,
		message: "Invalid declaration",
		info: [
			Spanned({
				span: _repr,
				message: "Tagged kinds may not have an underlaying type",
				isPrimary: true
			}),
			Spanned({
				span: kind.span,
				message: 'For kind `${kind.name.name}`',
				isSecondary: true
			})
		]
	},

	at(Type_NoValueCaseInit(vcase, _vcase, _init)) => {
		severity: Severity.ERROR,
		message: "Invalid value case",
		info: [
			Spanned({
				span: _init,
				message: "Value cases may not have an initializer",
				isPrimary: true
			}),
			Spanned({
				span: _vcase,
				message: 'For value case `${vcase}`',
				isSecondary: true
			})
		]
	},

	at(Type_DuplicateAttribute(decl, name, attr, _attr)) => {
		severity: Severity.ERROR,
		message: "Duplicate attribute",
		info: [
			Spanned({
				span: _attr,
				message: 'Duplicate attribute `is $attr`',
				isPrimary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `$name`',
				isSecondary: true
			})
		]
	},

	at(Type_InvalidAttribute(decl, name, attr, _attr)) => {
		severity: Severity.ERROR,
		message: "Invalid attribute",
		info: [
			Spanned({
				span: _attr,
				message: 'Invalid attribute `is $attr`',
				isPrimary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `$name`',
				isSecondary: true
			})
		]
	},

	at(Type_DuplicateDecl(decl, decl2)) => {
		severity: Severity.ERROR,
		message: "Duplicate declaration",
		info: [
			Spanned({
				span: decl2.span(),
				message: 'Duplicate ${decl2.name()}',
				isPrimary: true
			}),
			Spanned({
				span: decl.span,
				message: 'In ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	},
	at(Type_DuplicateDeclInFile(file, decl)) => {
		severity: Severity.ERROR,
		message: "Duplicate declaration",
		info: [
			Spanned({
				span: decl.span(),
				message: 'Duplicate ${decl.name()}',
				isPrimary: true
			})
		]
	},

	at(Type_UnexpectedDecl(decl, decl2)) => {
		severity: Severity.ERROR,
		message: "Unexpected declaration",
		info: [
			Spanned({
				span: decl2.span(),
				message: 'Unexpected ${decl2.name()}',
				isPrimary: true
			}),
			Spanned({
				span: decl.span,
				message: 'In ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	},
	at(Type_UnexpectedDeclInFile(file, decl)) => {
		severity: Severity.ERROR,
		message: "Unexpected declaration",
		info: [
			Spanned({
				span: decl.span(),
				message: 'Unexpected ${decl.name()}',
				isPrimary: true
			})
		]
	},

	at(Type_InvalidDecl(decl, decl2)) => {
		severity: Severity.ERROR,
		message: "Invalid declaration",
		info: [
			Spanned({
				span: decl2.span(),
				message: 'Invalid ${decl2.name()}',
				isPrimary: true
			}),
			Spanned({
				span: decl.span,
				message: 'In ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	},
	at(Type_InvalidDeclInFile(file, decl)) => {
		severity: Severity.ERROR,
		message: "Invalid declaration",
		info: [
			Spanned({
				span: decl.span(),
				message: 'Invalid ${decl.name()}',
				isPrimary: true
			})
		]
	},

	at(Type_InvalidTypeLookup(span, why)) => {
		severity: Severity.ERROR,
		message: "Invalid type lookup",
		info: [
			Spanned({
				span: span,
				message: why._or("Invalid type lookup"),
				isPrimary: true
			})
		]
	},

	at(Type_InvalidTypeApply(span, why)) => {
		severity: Severity.ERROR,
		message: "Invalid type application",
		info: [
			Spanned({
				span: span,
				message: why._or("Invalid type application"),
				isPrimary: true
			})
		]
	},

	at(Type_NotYetImplemented(span, why)) => {
		severity: Severity.ERROR,
		message: "Not yet implemented",
		info: [
			Spanned({
				span: span,
				message: why._or("This feature has not been implemented yet"),
				isPrimary: true
			})
		]
	},

	at(Type_DuplicateParam(method, name, origSpan, dupSpan)) => {
		severity: Severity.ERROR,
		message: "Duplicate parameter",
		info: [
			Spanned({
				span: dupSpan,
				message: 'Duplicate parameter `$name`',
				isPrimary: true
			}),
			Spanned({
				span: origSpan,
				message: 'First defined here',
				isSecondary: true
			}),
			Spanned({
				span: method.span,
				message: 'For ${method.declName()} `${method.methodName()}`',
				isSecondary: true
			})
		]
	},
	at(Type_DuplicateCaseParam(tcase, name, origSpan, dupSpan)) => {
		severity: Severity.ERROR,
		message: "Duplicate parameter",
		info: [
			Spanned({
				span: dupSpan,
				message: 'Duplicate parameter `$name`',
				isPrimary: true
			}),
			Spanned({
				span: origSpan,
				message: 'First defined here',
				isSecondary: true
			}),
			Spanned({
				span: tcase.span,
				message: 'For ${tcase.declName()} `${tcase.params.joinMap("", p -> p.label.name+":")}`',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownFieldOrVar(ctx, name, _name)) => {
		severity: Severity.ERROR,
		message: "Unknown name",
		info: [
			Spanned({
				span: _name,
				message: 'Unknown field or variable `$name`',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_ShadowedLocalVar(ctx, name, origSpan, dupSpan)) => {
		severity: Severity.WARNING,
		message: "Shadowed variable",
		info: [
			Spanned({
				span: dupSpan,
				message: 'This shadows an existing local variable `$name`',
				isPrimary: true
			}),
			Spanned({
				span: origSpan,
				message: 'First defined here',
				isSecondary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_LocalVarTypeMismatch(ctx, name, wantedType, gotType, declSpan, hereSpan)) => {
		severity: Severity.ERROR,
		message: "Type mismatch",
		info: [
			Spanned({
				span: hereSpan,
				message: 'local variable `$name` declared to be of type `${wantedType.fullName()}`, but got `${gotType.fullName()}` instead',
				isPrimary: true
			}),
			Spanned({
				span: declSpan,
				message: 'First defined here',
				isSecondary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownMethod(ctx, type, kind, span, categories)) => {
		severity: Severity.ERROR,
		message: "Unknown method",
		info: [
			Spanned({
				span: span,
				message: {
					var access: Access;
					final methodName = kind._match(
						at(Single(access_, name)) => {
							access = access_;
							'[$name]';
						},
						at(Multi(access_, names, null)) => {
							access = access_;
							"["+names.joinMap(" ", n -> '$n:')+"]";
						},
						at(Multi(access_, names, args!!)) => {
							access = access_;
							"["+names.zip(args, (n, a) -> '$n: (${a.t._andOr(t => t.fullName(), "???")})').join(" ")+"]";
						},
						at(Unary(op)) => {
							access = Instance;
							'[`${op.symbol()}`]';
						},
						at(Binary(op, null)) => {
							access = Instance;
							'[`${op.symbol()}`:]';
						},
						at(Binary(op, rtype!!)) => {
							access = Instance;
							'[`${op.symbol()}`: (${rtype.fullName()})]';
						}
					);

					var msg = '${access.desc()} `${type.fullName()}` does not respond to method $methodName';
					
					categories._and(cats => {
						msg += " in any categories of:";
						for(cat in cats) {
							msg += '\n    ${cat.fullName()}';
						}
					});

					msg;
				},
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownCast(ctx, type, target, span, categories)) => {
		severity: Severity.ERROR,
		message: "Unknown cast",
		info: [
			Spanned({
				span: span,
				message: {
					var msg = 'Value of type `${type.fullName()}` cannot be cast to type `${target.fullName()}`';
					
					categories._and(cats => {
						msg += " in any categories of:";
						for(cat in cats) {
							msg += '\n    ${cat.fullName()}';
						}
					});

					msg;
				},
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownGetter(ctx, access, type, name, span)) => {
		severity: Severity.ERROR,
		message: "Unknown name access",
		info: [
			Spanned({
				span: span,
				message: '${access.desc()} `${type.fullName()}` does not have member/getter `$name`',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownSetter(ctx, access, type, name, span, value)) => {
		severity: Severity.ERROR,
		message: "Unknown name access",
		info: [
			Spanned({
				span: span,
				message: {
					var msg = '${access.desc()} `${type.fullName()}` does not have member/setter `$name`';

					value._and(expr => {
						msg += ' of type ${expr.t._andOr(t=>t.fullName(), "???")}';
					});

					msg;
				},
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_UnknownCategory(ctx, access, type, cat, span)) => {
		severity: Severity.ERROR,
		message: "Unknown cast",
		info: [
			Spanned({
				span: span,
				message: '${access.desc()} `${type.fullName()}` does not have the category `${cat.fullName()}`',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_ThisNotAllowed(ctx, span)) => {
		severity: Severity.ERROR,
		message: "Invalid usage",
		info: [
			Spanned({
				span: span,
				message: '`this` is not allowed in a static context',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_ExpectedLogicalValue(ctx, gotType, span)) => {
		severity: Severity.ERROR,
		message: "Invalid type",
		info: [
			Spanned({
				span: span,
				message: 'Expected a logical value, but got value of type `${gotType.fullName()}` instead',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_PossiblyUnintendedArrowBlock(ctx, span)) => {
		severity: Severity.WARNING,
		message: "Possibly unintentional arrow shorthand",
		info: [
			Spanned({
				span: span,
				message: 'Using a block in an arrow shorthand does not act the same as a plain block!',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_ArrayPatternNotAllowed(ctx, span)) => {
		severity: Severity.ERROR,
		message: "Invalid pattern",
		info: [
			Spanned({
				span: span,
				message: 'This pattern is only allowed in array patterns',
				isPrimary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	at(Type_DuplicateBinding(ctx, name, origSpan, dupSpan)) => {
		severity: Severity.WARNING,
		message: "Duplicate binding",
		info: [
			Spanned({
				span: dupSpan,
				message: 'This shadows a previous binding `$name`',
				isPrimary: true
			}),
			Spanned({
				span: origSpan,
				message: 'First defined here',
				isSecondary: true
			}),
			Spanned({
				span: ctx.thisLookup.span,
				message: 'In ${ctx.description()}',
				isSecondary: true
			})
		]
	},

	_ => throw "todo"
)); }