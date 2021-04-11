package compiler;

@:build(util.Auto.build())
class Param {
	var type: Type;
	var name: Option<String>;
	var value: Option<Expr> = None;
}