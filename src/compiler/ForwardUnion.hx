package compiler;

import util.Buffer;

class ForwardUnion extends TypeDecl {
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString("union ");
		buf.addString(path.form());
		buf.addChar(";".code);
		
		return buf.toString();
	}
}