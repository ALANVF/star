package compiler;

import util.Buffer;

class Method extends NamedMethod {
	var ret: Type;
	var args: Array<Param>;
	var trailingRet: Bool = false;
	var hasVarargs: Bool = false;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		if(trailingRet) {
			buf.addString("auto ");
		} else {
			buf.addString(ret.form());
			buf.addChar(" ".code);
		}

		path.forEach(p -> {
			buf.addString(p.form());
			buf.addString("::");
		});
		
		buf.addString('$name(');
		buf.addString(args.map(a -> a.form()).join(", "));
		
		if(hasVarargs) {
			buf.addString(", ...");
		}
		
		buf.addChar(")".code);
		
		if(trailingRet) {
			buf.addString(" -> ");
			buf.addString(ret.form());
		}
		
		buf.addString(switch body {
			case None: ";";
			case Some(b): b.form(indent);
		});
		
		return buf.toString();
	}
}