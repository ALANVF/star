package compiler;

import util.Buffer;

@:build(util.Auto.build())
class ClassParent {
	var attrs: Array<String> = [];
	var type: Type;
	
	function form() {
		return attrs.map(a -> '$a ').join("") + type.form();
	}
}

//@:build(util.Auto.build())
@:publicFields
@:structInit
class ClassBody {
	var normal: DeclBody = [];
	var priv: DeclBody = [];
	var prot: DeclBody = [];
	var pub: DeclBody = [];
}

abstract class ClassLike extends TypeDecl {
	var parents: Array<ClassParent> = [];
	var body: ClassBody = {};
	
	private function _form(kw: String, indent) {
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent);
		final nws = "\n" + "\t".repeat(indent + 1);
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		buf.addString(kw);
		buf.addString(path.form());
		
		if(parents.length > 0) {
			buf.addString(": ");
			buf.addString(parents.map(p -> p.form()).join(", "));
		}
		
		buf.addString(" {");
		
		if(body.normal.length > 0) {
			for(decl in body.normal) {
				buf.addString(nws);
				buf.addString(decl.form(indent + 1));
			}
		}
		
		if(body.priv.length > 0) {
			buf.addString(ws);
			buf.addString("private:");
			for(decl in body.priv) {
				buf.addString(nws);
				buf.addString(decl.form(indent + 1));
			}
		}
		
		if(body.prot.length > 0) {
			buf.addString(ws);
			buf.addString("protected:");
			for(decl in body.prot) {
				buf.addString(nws);
				buf.addString(decl.form(indent + 1));
			}
		}
		
		if(body.pub.length > 0) {
			buf.addString(ws);
			buf.addString("public:");
			for(decl in body.pub) {
				buf.addString(nws);
				buf.addString(decl.form(indent + 1));
			}
		}
		
		buf.addString(ws);
		buf.addString("};");
		
		return buf.toString();
	}
}