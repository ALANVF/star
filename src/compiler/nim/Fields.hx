package compiler.nim;

@:using(compiler.nim.Fields.FieldsTools)
typedef Fields = Array<{n: Names, ?t: Type, ?pragmas: Pragmas, ?v: Expr}>;

final TAB = "    ";

@:publicFields
class FieldsTools {
	static function paramsToNim(self: Fields, tabs: String) {
		return "(" + self.inlineToNim(tabs) + ")";
	}
	
	static function inlineToNim(self: Fields, tabs: String) {
		return self.joinMap("; ", f -> {
			var res = f.n.toNim();
			f.pragmas._and(p => res += " " + p.toNim());
			res += ": " + f.t.toNim(tabs);
			f.v._and(v => res += " = " + v.toNim(tabs));
			res;
		});
	}
	
	static function toNim(self: Fields, tabs: String, noIndent = false) {
		var res = "";
		
		if(!noIndent) tabs += TAB;
		
		final ws = '\n$tabs';
		
		for(field in self) {
			res += ws;
			
			res += field.n.toNim();
			
			field.t._and(t => {
				res += ": ";
				res += t.toNim(tabs);
			});
			
			field.pragmas._and(p => {
				res += " ";
				res += p.toNim();
			});
			
			field.v._and(v => {
				res += " = ";
				res += v.toNim(tabs);
			});
		}
		
		return res;
	}
}