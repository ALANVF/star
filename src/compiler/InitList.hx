package compiler;

@:using(compiler.InitList.InitListTools)
enum InitList {
	LExprs(e: Array<Expr>);
	LNamed(n: Array<{name: String, expr: Expr}>);
	LNamedInit(b: Array<{name: String, init: InitList}>);
}


@:publicFields
class InitListTools {
	static function form(l: InitList, indent = 0) {
		return "{" + (switch l {
			case LExprs(exprs): exprs.map(e -> e.form(indent)).join(", ");
			case LNamed(names): names.map(n -> "." + ExprTools.fixName(n.name) + " = " + n.expr.form(indent)).join(", ");
			case LNamedInit(names): names.map(n -> "." + ExprTools.fixName(n.name) + n.init.form(indent)).join(", ");
		}) + "}";
	}
}