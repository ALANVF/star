package compiler;

import util.Buffer;

class EnumClass extends Enum {
	override function form(indent = 0) {
		return super.form(indent).replaceFirst("enum", "enum class");
	}
}