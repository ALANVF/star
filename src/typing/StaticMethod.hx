package typing;

abstract class StaticMethod extends AnyMethod {
	var ret: Option<Type>;
	var isMain: Bool = false;
	var isGetter: Bool = false;
	var isSetter: Bool = false;
	var isInline: Bool = false;
	var isMacro: Bool = false;

	static function fromAST(decl, ast: parsing.ast.decls.Method): Option<StaticMethod> return switch ast.spec.of {
		case Single(_): Some(SingleStaticMethod.fromAST(decl, ast));
		case Multi(_): Some(MultiStaticMethod.fromAST(decl, ast));
		case Cast(_):
			decl.errors.push(Errors.unexpectedDecl(decl, decl.name.name, DMethod(ast)));
			None;
	}

	inline function declName() {
		return "static method";
	}
}