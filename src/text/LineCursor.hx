package text;

import hx.strings.Char;

@:publicFields
@:structInit
class LineCursor {
	var column: Int = 0;
	var tabSize: Int = 4;

	function append(char: Char) {
		final isTab = char == Char.TAB;
		final advance = if(isTab) {
			tabSize - column % tabSize;
		} else if(!char.isAsciiControl()) {
			1;
		} else {
			0;
		};

		column += advance;

		return {isTab: isTab, advance: advance};
	}
}