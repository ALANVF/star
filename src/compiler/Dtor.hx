package compiler;

import util.Buffer;

class Dtor extends NamedMethod {
	function form(indent = 0) {
		final buf = new Buffer();
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		//buf.addString(type.form());
		//buf.addChar(" ".code);
		
		path.forEach(p -> {
			buf.addString(p.form());
			buf.addString("::");
		});
		
		buf.addString('~$name()');
		buf.addString(switch body {
			case None: ";";
			case Some(b): b.form(indent);
		});
		
		return buf.toString();
	}
}