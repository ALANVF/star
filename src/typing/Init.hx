package typing;

@:structInit
abstract class Init extends AnyMethod {
	var isMacro: Bool = false;
	
	static function fromAST(decl, ast: parsing.ast.decls.Init) return switch ast.spec.of {
		case Single(_): SingleInit.fromAST(decl, ast);
		case Multi(_): MultiInit.fromAST(decl, ast);
	}
	
	function declName() {
		return "initializer";
	}
}