package compiler;

import util.Buffer;

class Concept extends TypeDecl {
	var cond: Expr;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString("concept ");
		buf.addString(path.form());
		buf.addString(" = ");
		buf.addString(cond.form(indent));
		buf.addChar(";".code);
		
		return buf.toString();
	}
}