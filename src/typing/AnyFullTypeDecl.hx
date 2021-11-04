package typing;

@:build(util.Auto.build())
abstract class AnyFullTypeDecl extends AnyTypeDecl {
	var params: Array<Type>;
}