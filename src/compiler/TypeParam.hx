package compiler;

import util.Buffer;

@:build(util.Auto.build())
class TypeParam {
	var template: Option<Template> = None;
	var type: Option<Type> = None;
	var isVariadic: Bool = false;
	var name: Option<String> = None;
	var value: Option<Type> = None;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addChar(" ".code);
		});
		
		switch type {
			case None: buf.addString("typename");
			case Some(t): buf.addString(t.form());
		}
		
		if(isVariadic) {
			buf.addString("...");
		}
		
		name.forEach(n -> {
			buf.addChar(" ".code);
			buf.addString(n);
		});
		
		value.forEach(v -> {
			buf.addString(" = ");
			buf.addString(v.form());
		});
		
		return buf.toString();
	}
}