package compiler;

@:build(util.Auto.build())
class Member {
	var attrs: Attrs = [];
	var type: Type;
	var name: String;
	var bits: Option<Int> = None;
	var value: Option<Expr> = None;
}