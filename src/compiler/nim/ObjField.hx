package compiler.nim;

@:using(compiler.nim.ObjField.ObjFieldTools)
enum ObjField {
	FFields(fields: Fields);
	FWhen(cond: Expr, then: ObjFields, ?elifs: Array<Elif<ObjField>>, ?else_: ObjFields);
	FCase(name: Name, type: Type, cases: Array<Case<ObjField>>, ?else_: ObjFields);
	FDiscard;
	FNil;
}

final TAB = "    ";

@:publicFields
class ObjFieldTools {
	static function toNim(self: ObjField, tabs: String) return switch self {
		case FFields(fields): fields.toNim(tabs, true);
		case FWhen(cond, then, elifs, else_): {
			var res = "when ";
			
			res += cond.toNim(tabs);
			res += ":";
			res += then.toNim(tabs);
			
			elifs._and(e => for(elif in e) {
				res += '\n${tabs}elif ';
				res += elif.cond.toNim(tabs);
				res += ":";
				res += (elif.body : ObjFields).toNim(tabs);
			});
			
			else_._and(e => {
				res += '\n${tabs}else:';
				res += e.toNim(tabs);
			});
			
			res;
		}
		case FCase(name, type, cases, else_): {
			var res = "case ";
			
			res += name.toNim();
			res += ": ";
			res += type.toNim(tabs);
			
			final ws = '\n$tabs';
			
			for(c in cases) {
				res += ws;
				switch c {
					case COf(value, body):
						res += "of ";
						res += value.toNim(tabs);
						res += ":";
						res += (body : ObjFields).toNim(tabs);
					case COfAny(values, body):
						res += "of ";
						res += values.toNim(tabs);
						res += ":";
						res += (body : ObjFields).toNim(tabs);
					case CElif(cond, body):
						res += "elif ";
						res += cond.toNim(tabs);
						res += ":";
						res += (body : ObjFields).toNim(tabs);
				}
			}
			
			else_._and(e => {
				res += ws;
				res += "else:";
				res += e.toNim(tabs);
			});
			
			res;
		}
		case FDiscard: "discard";
		case FNil: "nil";
	}
}