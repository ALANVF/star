package typing;

import text.Span;

@:publicFields abstract class Local {
	var ctx: Ctx;
	var span: Span;
	var name: String;
	var type: Null<Type>;
	var expr: Null<TExpr>;
}