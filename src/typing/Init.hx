package typing;

abstract class Init extends AnyMethod {
	static function fromAST(decl, ast: parsing.ast.decls.Init) return switch ast.spec.of {
		case Single(_): SingleInit.fromAST(decl, ast);
		case Multi(_): MultiInit.fromAST(decl, ast);
	}
	
	inline function declName() {
		return "initializer";
	}
}