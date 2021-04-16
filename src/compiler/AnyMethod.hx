package compiler;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class AnyMethod {
	var attrs: Attrs = [];
	var path: Option<TypePath> = None;
	var body: Option<Body> = None;
	
	abstract function form(indent: Int = 0): String;
}