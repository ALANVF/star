package compiler;

import compiler.Member.MemberExprKind;
import util.Buffer;

@:publicFields
@:structInit
class MemberListPart {
	var name: String;
	var bits: Option<Expr> = None;
	var expr: MemberExprKind = MNone;
	
	function form(indent = 1) {
		final buf = new Buffer();
		
		buf.addString(name);
		
		bits.forEach(b -> {
			buf.addString(": ");
			buf.addString(b.form(indent));
		});
		
		buf.addString(expr.form(indent));
		
		return buf.toString();
	}
}

@:build(util.Auto.build())
class MemberList {
	var attrs: Attrs = [];
	var type: Type;
	var parts: Array<MemberListPart>;
	
	function form(indent = 0) {
		final buf = new Buffer();
		final ws = "\n" + "\t".repeat(indent + 1);
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		buf.addString(type.form());
		
		for(part in parts) {
			buf.addString(ws);
			buf.addString(part.form(indent + 1));
		}
		
		return buf.toString();
	}
}