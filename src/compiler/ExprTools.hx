package compiler;

import util.Buffer;

final ESCAPE_NAMES = [
	"alignas",
	"alignof",
	"and",
	"and_eq",
	"asm",
	"atomic_cancel",
	"atomic_commit",
	"atomic_noexcept",
	"auto",
	"bitand",
	"bitor",
	"bool",
	"break",
	"case",
	"catch",
	"char",
	"char8_t",
	"char16_t",
	"char32_t",
	"class",
	"compl",
	"concept",
	"const",
	"consteval",
	"constexpr",
	"constinit",
	"const_cast",
	"continue",
	"co_await",
	"co_return",
	"co_yield",
	"decltype",
	"default",
	"delete",
	"do",
	"double",
	"dynamic_cast",
	"else",
	"enum",
	"explicit",
	"export",
	"extern",
	"false",
	"float",
	"for",
	"friend",
	"goto",
	"if",
	"inline",
	"int",
	"long",
	"mutable",
	"namespace",
	"new",
	"noexcept",
	"not",
	"not_eq",
	"nullptr",
	"operator",
	"or",
	"or_eq",
	"private",
	"protected",
	"public",
	"reflexpr",
	"register",
	"reinterpret_cast",
	"requires",
	"return",
	"short",
	"signed",
	"sizeof",
	"static",
	"static_assert",
	"static_cast",
	"struct",
	"switch",
	"synchronized",
	"template",
	"this",
	"thread_local",
	"throw",
	"true",
	"try",
	"typedef",
	"typeid",
	"typename",
	"union",
	"unsigned",
	"using",
	"virtual",
	"void",
	"volatile",
	"wchar_t",
	"while",
	"xor",
	"xor_eq",
	
	"std"
];

@:publicFields
class ExprTools {
	@:noUsing
	static function fixName(name: String) {
		return if(ESCAPE_NAMES.contains(name)) {
			name + "$";
		} else {
			name;
		}
	}
	
	@:noUsing
	static inline function formExprs(exprs: Array<Expr>) {
		return exprs.map(e -> e.form()).join(", ");
	}
	
	static function form(expr: Expr) {
		return switch expr {
			case ENullptr: "nullptr";
			
			case EThis: "this";
			
			case EBool(true): "true";
			case EBool(false): "false";
			
			case EInt(i, None): '$i';
			case EInt(i, Some(exp)): '${i}e$exp';
			
			case EFloat(i, f, None): '$i.$f';
			case EFloat(i, f, Some(exp)): '$i.${f}e$exp';
			
			case EChar(c): switch c {
				case "'".code | '\\'.code: '\\$c';
				case '\n'.code: '\\n';
				case '\r'.code: '\\r';
				case '\t'.code: '\\t';
				case 0|1|2|3|4|5|6|7|8|11|12|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31:
					"\\0x" + hx.strings.Strings.toHex(c, 2);
				case c: c.toString();
			}
			
			case EString(s):
				var buf = new Buffer();
				
				buf.addChar('"'.code);
				
				for(i in 0...s.length) switch StringTools.unsafeCodeAt(s, i) {
					case c = '"'.code | '\\'.code:
						buf.addChar('\\'.code);
						buf.addChar(c);
					case '\n'.code: buf.addString('\\n');
					case '\r'.code: buf.addString('\\r');
					case '\t'.code: buf.addString('\\t');
					case c = (0|1|2|3|4|5|6|7|8|11|12|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31):
						buf.addString("\\x");
						buf.addString(hx.strings.Strings.toHex(c, 2));
					case c: buf.addChar(c);
				}
				
				buf.addChar('"'.code);
				
				buf.toString();
			
			case EName(n): fixName(n);
			
			case EParen(e): "(" + e.form() + ")";
			
			case EInitList(i): i.form();
			
			case ETypeCtor(type, ctor): type.form() + ctor.form();
			
			case ELambda(captures, template, params, attrs, ret, body): throw "todo!";
			
			case ENew(None, None, t, false, ctor): "new " + t.form() + ctor.map(c -> c.form()).orElse("");
			case ENew(None, None, t, true, ctor): "new (" + t.form() + ")" + ctor.map(c -> c.form()).orElse("");
			case ENew(_, _, _, _, _): throw "todo!";
			
			case EDelete(true, expr): "delete[] " + expr.form();
			case EDelete(false, expr): "delete " + expr.form();
			
			case EThrow(None): "throw";
			case EThrow(Some(expr)): "throw " + expr.form();
			
			case ECast(type, expr): "((" + type.form() + ")(" + expr.form() + "))";
			
			case EConstCast(type, expr): "const_cast<" + type.form() + ">(" + expr.form() + ")";
			
			case EStaticCast(type, expr): "static_cast<" + type.form() + ">(" + expr.form() + ")";
			
			case EDynamicCast(type, expr): "dynamic_cast<" + type.form() + ">(" + expr.form() + ")";
			
			case EReinterpretCast(type, expr): "reinterpret_cast<" + type.form() + ">(" + expr.form() + ")";
			
			case EPrefix(op, r): op.form() + r.form();
			
			case ESuffix(l, op): l.form() + op.form();
			
			case EInfix(l, op, r): l.form() + op.form() + r.form();
			
			case EIndex(expr, index): expr.form() + "[" + index.form() + "]";
			
			case ECall(expr, None, args): expr.form() + "(" + formExprs(args) + ")";
			case ECall(expr, Some(params), args): expr.form() + "<" + params.formTypes() + ">(" + formExprs(args) + ")";
			
			case EDot(expr, name): expr.form() + "." + fixName(name);
			case EDotStatic(expr, path, name): expr.form() + "." + path.form() + "::" + fixName(name);
			case EDotTemplate(expr, name): expr.form() + ".template " + fixName(name);
			case EDotRef(l, r): l.form() + ".*" + r.form();
			
			case EArrow(expr, name): expr.form() + "->" + fixName(name);
			case EArrowStatic(expr, path, name): expr.form() + "->" + path.form() + "::" + fixName(name);
			case EArrowTemplate(expr, name): expr.form() + "->template " + fixName(name);
			case EArrowRef(l, r): l.form() + "->*" + r.form();
			
			case EScope(type, name): type.form() + "::" + fixName(name);
			
			case ETernary(cond, yes, no): cond.form() + " ? " + yes.form() + " : " + no.form();
			
			case ESizeof(expr): "sizeof(" + expr.form() + ")";
			case ESizeofPack(expr): "sizeof...(" + expr.form() + ")";
			
			case EAlignof(type): "alignof(" + type.form() + ")";
			
			case ETypeid(expr): "typeid(" + expr.form() + ")";
			
			case ERequires(req): req.form();
			
			case EType(t): t.form();
			
			case ERaw(code): code;
		}
	}
}