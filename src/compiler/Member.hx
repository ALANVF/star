package compiler;

@:build(util.Auto.build())
class Member {
	var template: Option<Template> = None;
	var attrs: Attrs = [];
	var type: Type;
	var path: Option<TypePath> = None;
	var name: String;
	var bits: Option<Int> = None;
	var value: Option<Expr> = None;
}