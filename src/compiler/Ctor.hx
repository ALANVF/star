package compiler;

import util.Buffer;

class Ctor extends NamedMethod {
	var args: Array<Param>;
	var hasVarargs: Bool = false;
	var inits: Array<MemberInit>;
	
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
		
		buf.addString('$name(');
		buf.addString(args.map(a -> a.form()).join(", "));
		
		if(hasVarargs) {
			buf.addString(", ...");
		}
		
		buf.addChar(")".code);
		
		if(inits.length > 0) {
			buf.addString(": ");
			buf.addString(inits.map(i -> switch i {
				case MVar(name, ctor): name + ctor.form();
				case MCtor(type, ctor): type.form() + ctor.form();
			}).join(", "));
		}
		
		buf.addString(switch body {
			case None: ";";
			case Some(b): b.form(indent);
		});
		
		return buf.toString();
	}
}