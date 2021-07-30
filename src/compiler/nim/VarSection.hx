package compiler.nim;

@:using(compiler.nim.VarSection.VarSectionTools)
typedef VarSection = Array<{n: Names, ?unpack: Bool, ?t: Type, ?pragmas: Pragmas, ?v: Expr}>;

final TAB = "    ";

@:publicFields
class VarSectionTools {
	static function toNim(self: VarSection, tabs: String) {
		var res = "";
		
		tabs += TAB;
		
		final ws = '\n$tabs';
		
		for(sec in self) {
			res += ws;
			
			final names = sec.n.toNim();
			res += sec.unpack ? '($names)' : names;
			
			sec.t._and(t => {
				res += ": ";
				res += t.toNim(tabs);
			});
			
			sec.pragmas._and(p => {
				res += " ";
				res += p.toNim();
			});
			
			sec.v._and(v => {
				res += " = ";
				res += v.toNim(tabs);
			});
		}
		
		return res;
	}
}