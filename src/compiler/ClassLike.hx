package compiler;

@:build(util.Auto.build())
class ClassParent {
	var attrs: Array<String> = [];
	var type: Type;
}

@:build(util.Auto.build())
class ClassBody {
	var normal: DeclBody = [];
	var priv: DeclBody = [];
	var prot: DeclBody = [];
	var pub: DeclBody = [];
}

abstract class ClassLike extends TypeDecl {
	var parents: Array<ClassParent> = [];
	var body: ClassBody = new ClassBody();
}