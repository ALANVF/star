package typing;

class DirectAlias extends TypeDecl {
	var type: Type;

	inline function declName() {
		return "alias";
	}
}