package compiler;

@:build(util.Auto.build())
class Param {
	var type: Type;
	var name: Option<String>;
	var value: Option<Expr> = None;
	
	function form() {
		return (
			type.form()
			+
			name.map(n -> ' $n').orElse("")
			+
			value.map(v -> " = " + v.form()).orElse("")
		);
	}
}