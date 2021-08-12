package typing;

abstract class Alias extends TypeDecl {
	var isHidden: Bool = false;

	static function fromAST(lookup, ast: parsing.ast.decls.Alias): Alias {
		return switch ast.kind {
			case Direct(_, _): DirectAlias.fromAST(lookup, ast);
			case Strong(_, _): StrongAlias.fromAST(lookup, ast);
			case Opaque(_): OpaqueAlias.fromAST(lookup, ast);
		}
	}

	function declName() {
		return "alias";
	}
}