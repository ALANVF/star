package compiler;

import util.Buffer;

@:build(util.Auto.build())
class Template {
	var types: Array<TypeParam>;
	var requires: Option<Expr> = None;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		buf.add("template<");
		buf.add(types.map(t -> t.form(indent)).join(", "));
		
		switch requires {
			case None: buf.addChar(">".code);
			case Some(r):
				buf.addString("> requires ");
				buf.addString(r.form(indent));
		}
		
		return buf.toString();
	}
}