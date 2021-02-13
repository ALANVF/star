package typing;

abstract class StaticMethod extends AnyMethod implements IMethod {
	var ret: Option<Type>;
	var isMain: Bool = false;
	var isGetter: Bool = false;
	var isSetter: Bool = false;
	var isUnordered: Bool = false;
	var isInline: Bool = false;

	static inline function fromAST(decl, ast: parsing.ast.decls.Method) return switch ast.spec.of {
		case Single(_): Some(SingleStaticMethod.fromAST(decl, ast));
		case Multi(_): throw "NYI!";
		case Cast(_):
			decl.errors.push(Errors.unexpectedDecl(decl, decl.name.name, DMethod(ast)));
			None;
	}

	inline function declName() {
		return "static method";
	}
}