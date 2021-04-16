package util;

import hx.strings.Strings.CharIndex;
import hx.strings.Char;
import hx.strings.StringBuilder;

class StringBuilders {
	public static function setChar(builder: StringBuilder, index: CharIndex, char: Char) {
		final old = builder.toString();
		builder
			.clear()
			.add(old.substr8(0, index))
			.addChar(char)
			.add(old.substr8(index + 1));
	}
}