package compiler;

@:build(util.Auto.build())
class Template {
	var types: Array<TypeParam>;
	var requires: Option<Expr> = None;
}