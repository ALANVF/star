package typing;

import errors.Error;
import text.Span;

abstract class Operator extends AnyMethod {
	var ret: Null<Type>;
	var opSpan: Span;
	var isInline: Bool = false;
	var isMacro: Bool = false;

	static function fromAST(decl: AnyTypeDecl, ast: parsing.ast.decls.Operator) {
		final oper: Operator = ast.spec._match(
			at(null) => {
				final op: UnaryOp = switch ast.symbol {
					case "++": Incr;
					case "--": Decr;
					case "-": Neg;
					case "!": Not;
					case "~": Compl;
					case "?": Truthy;
					case "+" | "*" | "**" | "/" | "//" | "%" | "%%" | "&" | "|" | "^" | "<<" | ">>" | "?=" | "!=" | ">" | ">=" | "<" | "<=" | "&&" | "||" | "^^" | "!!":
						decl.errors.push(Type_OpNeedsParameter(decl, ast));
						return None;
					case "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "%%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "&&=" | "||=" | "^^=" | "!!=":
						decl.errors.push(Type_OpNotOverloadable(decl, ast, false));
						return None;
					case "...":
						decl.errors.push(Type_OpNotOverloadable(decl, ast, true));
						return None;
					default:
						decl.errors.push(Type_UnknownOpOverload(decl, ast));
						return None;
				};

				UnaryOperator.fromAST(decl, op, ast);
			},

			at({of: {name: name, type: type}}) => {
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
						decl.errors.push(Type_OpDoesNotNeedParameter(decl, ast));
						return None;
					case "+=" | "-=" | "*=" | "**=" | "/=" | "//=" | "%=" | "%%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "&&=" | "||=" | "^^=" | "!!=":
						decl.errors.push(Type_OpNotOverloadable(decl, ast, false));
						return None;
					case "...":
						decl.errors.push(Type_OpNotOverloadable(decl, ast, true));
						return None;
					default:
						decl.errors.push(Type_UnknownOpOverload(decl, ast));
						return None;
				};

				BinaryOperator.fromAST(decl, op, name, type, ast);
			}
		);

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(oper.hidden != null): oper.errors.push(Type_DuplicateAttribute(oper, oper.opName(), "hidden", span));
			case IsHidden(None): oper.hidden = None;
			case IsHidden(Some(outsideOf)): oper.hidden = Some(decl.makeTypePath(outsideOf));

			case IsNoinherit: oper.noInherit = true;

			case IsNative(_) if(oper.native != null): oper.errors.push(Type_DuplicateAttribute(oper, oper.opName(), "native", span));
			case IsNative(sym): oper.native = sym;

			case IsInline: oper.isInline = true;

			case IsAsm: oper.isAsm = true;

			case IsMacro: oper.isMacro = true;
		}

		return Some(oper);
	}

	function declName() {
		return "operator overload";
	}

	abstract function opName(): String;
}