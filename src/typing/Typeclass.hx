package typing;

class Typeclass extends TypeDecl {
	var generic: Generic;

	inline function declName() {
		return "typeclass";
	}

	override function hasErrors() {
		return super.hasErrors() || generic.hasErrors();
	}

	override function allErrors() {
		return super.allErrors().concat(generic.allErrors());
	}
}