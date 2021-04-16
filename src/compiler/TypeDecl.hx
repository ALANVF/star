package compiler;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class TypeDecl {
	var template: Option<Template> = None;
	var path: TypePath;
	
	abstract function form(indent: Int = 0): String;
}