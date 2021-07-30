package compiler.nim;

@:using(compiler.nim.TypeSection.TypeSectionTools)
typedef TypeSection = Array<{
	name: Name,
	?params: TypeParams,
	?pragmas: Pragmas,
	decl: TypeDecl
}>;

final TAB = "    ";

@:publicFields
class TypeSectionTools {
	static function toNim(self: TypeSection, tabs: String) {
		var res = "";
		
		tabs += TAB;
		
		final ws = '\n$tabs';
		
		for(sec in self) {
			res += ws;
			
			res += sec.name.toNim();
			//res += "*";
			
			sec.params._and(p => {
				res += p.toNim(tabs);
			});
			
			sec.pragmas._and(p => {
				res += " ";
				res += p.toNim();
			});
			
			sec.decl._and(d => {
				res += " = ";
				res += d.toNim(tabs);
			});
		}
		
		return res;
	}
}