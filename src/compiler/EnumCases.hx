package compiler;

import util.Buffer;

@:using(compiler.EnumCases.EnumCasesTools)
typedef EnumCases = Array<{name: String, value: Option<Expr>}>;


@:publicFields
class EnumCasesTools {
	static function form(cases: EnumCases, indent = 0) {
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent + 1);
		
		buf.addChar("{".code);
		
		for(i => c in cases) {
			buf.addString(ws);
			buf.addString(c.name);
			
			c.value.forEach(v -> {
				buf.addString(" = ");
				buf.addString(v.form(indent));
			});
			
			if(i != cases.length - 1) {
				buf.addChar(",".code);
			}
		}
		
		buf.addString("\n" + "\t".repeat(indent));
		buf.addChar("}".code);
		
		return buf.toString();
	}
}