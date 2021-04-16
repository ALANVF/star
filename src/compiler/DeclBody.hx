package compiler;

import util.Buffer;

@:using(compiler.DeclBody.DeclBodyTools)
typedef DeclBody = Array<DeclStmt>;

@:publicFields
class DeclBodyTools {
	static function form(body: DeclBody, indent = 0) {
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent + 1);
		
		buf.addChar("{".code);
		
		for(stmt in body) {
			buf.addString(ws);
			buf.addString(stmt.form(indent + 1));
		}
		
		buf.addChar("\n".code);
		buf.addString("\t".repeat(indent));
		buf.addChar("}".code);
		
		return buf.toString();
	}
}