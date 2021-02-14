package typing;

abstract class Init extends AnyMethod {
	static inline function fromAST(decl, ast: parsing.ast.decls.Init) return switch ast.spec.of {
		case Single(_): Some(SingleInit.fromAST(decl, ast));
		case Multi(_): throw "NYI!";
	}
	
	inline function declName() {
		return "initializer";
	}
}