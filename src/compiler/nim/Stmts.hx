package compiler.nim;

@:using(compiler.nim.Stmts.StmtsTools)
typedef Stmts = Array<Stmt>;

final TAB = "    ";

@:publicFields
class StmtsTools {
	static function toNim(self: Stmts, tabs = "") {
		var res = "";
		
		tabs += TAB;
		
		final ws = '\n$tabs';
		
		for(stmt in self) {
			res += ws;
			res += stmt.toNim(tabs);
		}
		
		return res;
	}
}