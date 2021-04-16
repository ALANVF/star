package compiler;

import util.Buffer;

class Enum extends TypeDecl {
	var base: Option<Type>;
	var cases: EnumCases;
	
	function form(indent = 0) {
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent);
		final nws = "\n" + "\t".repeat(indent + 1);
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString(ws);
		});
		
		buf.addString("enum ");
		buf.addString(path.form());
		
		base.forEach(b -> {
			buf.addString(": ");
			buf.addString(b.form());
		});
		
		buf.addString(" {");
		
		for(i => c in cases) {
			buf.addString(nws);
			buf.addString(c.name);
			
			c.value.forEach(v -> {
				buf.addString(" = ");
				buf.addString(v.form(indent));
			});
			
			if(i != cases.length - 1) {
				buf.addChar(",".code);
			}
		}
		
		buf.addString(ws);
		buf.addString("};");
		
		return buf.toString();
	}
}