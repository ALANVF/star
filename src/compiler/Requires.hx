package compiler;

import util.Buffer;

using hx.strings.Strings;

enum RequiresStmt {
	RStmt(s: Stmt);
	RReq(e: Expr, t: Option<Type>);
	RType(t: Type);
	RNested(r: Expr);
}

typedef RequiresBody = Array<RequiresStmt>;

@:build(util.Auto.build())
class Requires {
	var args: Array<Param>;
	var body: RequiresBody;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		if(args.length == 0) {
			buf.addString("requires {");
		} else {
			buf.addString("requires(");
			buf.addString(args.map(a -> a.form()).join(", "));
			buf.addString(") {");
		}
		
		final ws = "\n" + "\t".repeat(indent + 1);
		
		for(stmt in body) {
			buf.addString(ws);
			buf.addString(switch stmt {
				case RStmt(s): s.form(indent + 1);
				case RReq(e, None): "{ " + e.form() + " };";
				case RReq(e, Some(t)): "{ " + e.form() + "} -> " + t.form() + ";";
				case RType(t): "typename " + t.form() + ";";
				case RNested(r): "requires " + r.form(indent) + ";";
			});
		}
		
		buf.addChar("\n".code);
		buf.addString("\t".repeat(indent));
		buf.addChar("}".code);
		
		return buf.toString();
	}
}