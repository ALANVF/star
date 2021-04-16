package compiler;

import util.Buffer;
using hx.strings.Strings;

@:using(compiler.Block.BlockTools)
typedef Block = Array<Stmt>;


@:publicFields
class BlockTools {
	static function form(block: Block, indent = 0) {
		if(block.length == 0) return "{}";
		
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent + 1);
		
		buf.addChar("{".code);
		
		for(stmt in block) {
			buf.addString(ws);
			buf.addString(stmt.form(indent + 1));
		}
		
		buf.addChar("\n".code);
		buf.addString("\t".repeat(indent));
		buf.addChar("}".code);
		
		return buf.toString();
	}
}