package compiler;

import util.Buffer;

class ForwardEnumClass extends TypeDecl {
	var base: Option<Type> = None;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString("enum class ");
		buf.addString(path.form());
		
		base.forEach(b -> {
			buf.addString(": ");
			buf.addString(b.form());
		});
		
		buf.addChar(";".code);
		
		return buf.toString();
	}
}