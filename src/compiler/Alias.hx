package compiler;

import util.Buffer;

class Alias extends TypeDecl {
	var type: Type;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString("using ");
		buf.addString(path.form());
		buf.addString(" = ");
		buf.addString(type.form());
		buf.addChar(";".code);
		
		return buf.toString();
	}
}