package compiler;

import util.Buffer;

@:using(compiler.Member.MemberExprKindTools)
enum MemberExprKind {
	MNone;
	MAssign(expr: Expr);
	MCall(args: Array<Expr>);
	MInitList(l: InitList);
}

@:build(util.Auto.build())
class Member {
	var template: Option<Template> = None;
	var attrs: Attrs = [];
	var type: Type;
	var path: Option<TypePath> = None;
	var name: String;
	var bits: Option<Expr> = None;
	var expr: MemberExprKind = MNone;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		template.forEach(t -> {
			buf.addString(t.form(indent));
			buf.addString("\n" + "\t".repeat(indent));
		});
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		buf.addString(type.form());
		buf.addChar(" ".code);
		
		path.forEach(p -> {
			buf.addString(p.form());
			buf.addString("::");
		});
		
		buf.addString(name);
		
		bits.forEach(b -> {
			buf.addString(": ");
			buf.addString(b.form(indent));
		});
		
		buf.addString(expr.form(indent));
		
		buf.addChar(";".code);
		
		return buf.toString();
	}
}


@:publicFields
class MemberExprKindTools {
	static function form(expr: MemberExprKind, indent = 0) return switch expr {
		case MNone: "";
		case MAssign(e): " = " + e.form(indent);
		case MCall(args): "(" + ExprTools.formExprs(args) + ")";
		case MInitList(l): l.form(indent);
	}
}