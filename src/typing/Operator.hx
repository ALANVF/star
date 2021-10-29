package typing;

import reporting.Severity;
import reporting.Diagnostic;
import text.Span;
import typing.Traits;

private inline function notOverloadable(decl: ITypeDecl, ast: parsing.ast.decls.Operator, yet = false) {
	return new Diagnostic({
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: ast.symbolSpan,
				message: 'The `${ast.symbol}` operator cannot be overloaded' + (yet? " (yet)" : ""),
				isPrimary: true
			}),
			Spanned({
				span: ast.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	});
}

private inline function needsParameter(decl: ITypeDecl, ast: parsing.ast.decls.Operator) {
	return new Diagnostic({
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: ast.symbolSpan,
				message: 'Overloading the `${ast.symbol}` operator requires a parameter',
				isPrimary: true
			}),
			Spanned({
				span: ast.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	});
}

private inline function excludeParameter(decl: ITypeDecl, ast: parsing.ast.decls.Operator) {
	return new Diagnostic({
		severity: Severity.ERROR,
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: ast.symbolSpan,
				message: 'Overloading the `${ast.symbol}` operator should not require a parameter',
				isPrimary: true
			}),
			Spanned({
				span: ast.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	});
}

private inline function unknownOp(decl: ITypeDecl, ast: parsing.ast.decls.Operator) {
	return new Diagnostic({
		message: "Invalid operator overload",
		info: [
			Spanned({
				span: ast.symbolSpan,
				message: 'The `${ast.symbol}` operator cannot be overloaded because it does not exist',
				isPrimary: true
			}),
			Spanned({
				span: ast.span,
				isSecondary: true
			}),
			Spanned({
				span: decl.span,
				message: 'For ${decl.declName()} `${decl.name.name}`',
				isSecondary: true
			})
		]
	});
}

abstract class Operator extends AnyMethod {
	var ret: Option<Type>;
	final opSpan: Span;
	var isInline: Bool = false;
	var isMacro: Bool = false;

	static function fromAST(decl: ITypeDecl, ast: parsing.ast.decls.Operator) {
		final oper: Operator = switch ast.spec {
			case None:
				final op: UnaryOp = switch ast.symbol {
					case "++": Incr;
					case "--": Decr;
					case "-": Neg;
					case "!": Not;
					case "~": Compl;
					case "?": Truthy;
					case "+" | "*" | "**" | "/" | "//" | "%" | "%%" | "&" | "|" | "^" | "<<" | ">>" | "?=" | "!=" | ">" | ">=" | "<" | "<=" | "&&" | "||" | "^^" | "!!":
						decl.errors.push(needsParameter(decl, ast));
						return None;
					case "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "%%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "&&=" | "||=" | "^^=" | "!!=":
						decl.errors.push(notOverloadable(decl, ast));
						return None;
					case "...":
						decl.errors.push(notOverloadable(decl, ast, true));
						return None;
					default:
						decl.errors.push(unknownOp(decl, ast));
						return None;
				};

				UnaryOperator.fromAST(decl, op, ast);

			case Some({of: {name: name, type: type}}):
				final op: BinaryOp = switch ast.symbol {
					case "+": Plus;
					case "-": Minus;
					case "*": Times;
					case "**": Pow;
					case "/": Div;
					case "//": IntDiv;
					case "%": Mod;
					case "%%": IsMod;
					case "&": BitAnd;
					case "|": BitOr;
					case "^": BitXor;
					case "<<": Shl;
					case ">>": Shr;
					case "?=": Eq;
					case "!=": Ne;
					case ">": Gt;
					case ">=": Ge;
					case "<": Lt;
					case "<=": Le;
					case "&&": And;
					case "||": Or;
					case "^^": Xor;
					case "!!": Nor;
					case "++" | "--" | "!" | "~" | "?":
						decl.errors.push(excludeParameter(decl, ast));
						return None;
					case "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "%%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "&&=" | "||=" | "^^=" | "!!=":
						decl.errors.push(notOverloadable(decl, ast));
						return None;
					case "...":
						decl.errors.push(notOverloadable(decl, ast, true));
						return None;
					default:
						decl.errors.push(unknownOp(decl, ast));
						return None;
				};

				BinaryOperator.fromAST(decl, op, name, type, ast);
		};

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(oper.hidden.isSome()): oper.errors.push(Errors.duplicateAttribute(oper, oper.opName(), "hidden", span));
			case IsHidden(None): oper.hidden = Some(None);
			case IsHidden(Some(outsideOf)): oper.hidden = Some(Some(decl.makeTypePath(outsideOf)));

			case IsNoinherit: oper.noInherit = true;

			case IsNative(_) if(oper.native.isSome()): oper.errors.push(Errors.duplicateAttribute(oper, oper.opName(), "native", span));
			case IsNative(sym): oper.native = Some(sym);

			case IsInline: oper.isInline = true;

			case IsAsm: oper.isAsm = true;

			case IsMacro: oper.isMacro = true;
		}

		return Some(oper);
	}

	inline function declName() {
		return "operator overload";
	}

	abstract function opName(): String;
}