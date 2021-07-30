package compiler.nim;

@:using(compiler.nim.Routine.RoutineTools)
typedef Routine = {
	name: Name,
	?typeParams: TypeParams,
	?params: Fields,
	?ret: Type,
	?pragmas: Pragmas,
	?body: Stmts
}

final TAB = "    ";

@:publicFields
class RoutineTools {
	static function toNim(self: Routine, tabs: String) {
		var res = self.name.toNim(); // + "*";
		
		self.typeParams._and(tp => {
			res += tp.toNim(tabs);
		});
		
		self.params._and(p => {
			res += p.paramsToNim(tabs);
		});
		
		self.ret._and(r => {
			res += ": ";
			res += r.toNim(tabs);
		});
		
		self.pragmas._and(p => {
			res += " ";
			res += p.toNim();
		});
		
		self.body._and(b => {
			res += " =";
			res += b.toNim(tabs);
		});
		
		return res;
	}
}