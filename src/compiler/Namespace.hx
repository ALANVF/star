package compiler;

@:build(util.Auto.build())
class Namespace {
	var path: Option<TypePath>;
	var body: DeclBody;
}