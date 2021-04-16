package compiler;

import util.Buffer;

class Enum extends TypeDecl {
	var base: Option<Type>;
	var cases: EnumCases;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString("enum ");
		buf.addString(path.form());
		
		base.forEach(b -> {
			buf.addString(": ");
			buf.addString(b.form());
		});
		
		buf.addChar(" ".code);
		buf.addString(cases.form(indent));
		buf.addChar(";".code);
		
		return buf.toString();
	}
}