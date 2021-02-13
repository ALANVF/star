package typing;

abstract class Namespace extends TypeDecl implements ITypeDecls {
	final decls: Array<TypeDecl> = [];

	override function hasErrors() {
		return super.hasErrors() || decls.some(d -> d.hasErrors());
	}

	override function allErrors() {
		return super.allErrors().concat(decls.flatMap(decl -> decl.allErrors()));
	}
}