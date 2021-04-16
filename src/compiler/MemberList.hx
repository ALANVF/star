package compiler;

@:publicFields
@:structInit
class MemberListPart {
	var name: String;
	var bits: Option<Int> = None;
	var value: Option<Expr> = None;
}

@:build(util.Auto.build())
class MemberList {
	var attrs: Attrs = [];
	var type: Type;
	var parts: Array<MemberListPart>;
}