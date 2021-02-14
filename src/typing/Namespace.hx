package typing;

abstract class Namespace extends TypeDecl
	implements ITypeDecls
	implements IStaticMembers
	implements IStaticMethods
	implements IStaticInit
	implements IStaticDeinit
{
	final decls: Array<TypeDecl> = [];
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	var staticInit: Option<StaticInit> = None;
	var staticDeinit: Option<StaticDeinit> = None;

	override function hasErrors() {
		return super.hasErrors() || decls.some(d -> d.hasErrors())
			|| staticMembers.some(m -> m.hasErrors()) || staticMethods.some(m -> m.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(decl in decls) result = result.concat(decl.allErrors());
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());

		return result;
	}
}