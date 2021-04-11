package compiler;

@:build(util.Auto.build())
class Parent {
	var attrs: Array<String> = [];
	var type: Type;
}

@:build(util.Auto.build())
class Body {
	var normal: DeclBody = [];
	var priv: DeclBody = [];
	var prot: DeclBody = [];
	var pub: DeclBody = [];
}

abstract class ClassLike extends TypeDecl {
	var parents: Array<Parent> = [];
	var body: Body = new Body();
}