package typing;

abstract class Method extends AnyMethod {
	var ret: Null<Type>;
	var isMain: Bool = false;
	var isGetter: Bool = false;
	var isSetter: Bool = false;
	var isInline: Bool = false;
	var isMacro: Bool = false;

	static function fromAST(decl, ast: parsing.ast.decls.Method): Method return switch ast.spec.of {
		case Single(_): SingleMethod.fromAST(decl, ast);
		case Multi(_): MultiMethod.fromAST(decl, ast);
		case Cast(_): CastMethod.fromAST(decl, ast);
	}

	function declName() {
		return "method";
	}
}