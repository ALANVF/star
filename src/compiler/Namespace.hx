package compiler;

import util.Buffer;

@:build(util.Auto.build())
class Namespace {
	var path: Option<TypePath>;
	var body: DeclBody;
	
	function form(indent = 0) {
		return switch path {
			case None: "namespace " + body.form(indent);
			case Some(p): "namespace " + p.form() + " " + body.form(indent);
		}
	}
}