package compiler;

import util.Buffer;

class Typedef extends TypeDecl {
	var type: Type;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		buf.addString("typedef ");
		buf.addString(type.form());
		buf.addChar(" ".code);
		buf.addString(path.form());
		buf.addChar(";".code);
		
		return buf.toString();
	}
}