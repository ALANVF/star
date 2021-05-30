package compiler;

class EnumClass extends Enum {
	override function form(indent = 0) {
		return super.form(indent).replaceFirst("enum ", "enum class ");
	}
}