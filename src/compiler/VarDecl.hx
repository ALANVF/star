package compiler;

import util.Buffer;

enum VarDeclKind {
	VVar(name: String);
	VUnpack(names: Array<String>);
}

enum VarDeclExprKind {
	VNone;
	VAssign(expr: Expr);
	VCall(args: Array<Expr>);
	VInitList(l: InitList);
}

@:build(util.Auto.build())
class VarDecl {
	var attrs: Attrs = [];
	var type: Type;
	var kind: VarDeclKind;
	var expr: VarDeclExprKind;
	
	function form(indent = 0) {
		final buf = new Buffer();
		
		for(attr in attrs) {
			buf.addString('$attr ');
		}
		
		buf.addString(type.form());
		buf.addChar(" ".code);
		
		switch kind {
			case VVar(name): buf.addString(ExprTools.fixName(name));
			case VUnpack(names):
				buf.addChar("[".code);
				buf.addString(names.map(ExprTools.fixName).join(", "));
				buf.addChar("]".code);
		}
		
		buf.addString(switch expr {
			case VNone: "";
			case VAssign(e): " = " + e.form(indent);
			case VCall(args): "(" + ExprTools.formExprs(args) + ")";
			case VInitList(l): l.form(indent);
		});
		
		return buf.toString();
	}
}