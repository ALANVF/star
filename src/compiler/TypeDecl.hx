package compiler;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class TypeDecl {
	var template: Option<String> = None;
	var path: TypePath;
}