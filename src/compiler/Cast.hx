package compiler;

import util.Buffer;

class Cast extends AnyMethod {
	var template: Option<Template> = None;
	var type: Type;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		path.forEach(p -> {
			buf.addString(p.form());
			buf.addString("::");
		});
		
		buf.addString("operator ");
		buf.addString(type.form());
		buf.addString("()");
		
		buf.addString(switch body {
			case None: ";";
			case Some(b): b.form(indent);
		});
		
		return buf.toString();
	}
}