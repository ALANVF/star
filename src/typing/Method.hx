package typing;

abstract class Method extends AnyMethod implements IMethod {
	var ret: Option<Type>;
	var isMain: Bool = false;
	var isGetter: Bool = false;
	var isSetter: Bool = false;
	var isUnordered: Bool = false;
	var isInline: Bool = false;

	static inline function fromAST(decl, ast: parsing.ast.decls.Method) return switch ast.spec.of {
		case Single(_): Some(SingleMethod.fromAST(decl, ast));
		case Multi(_): throw "NYI!";
		case Cast(_): throw "NYI!";
	}

	inline function declName() {
		return "method";
	}
}