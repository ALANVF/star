package text;

import hx.strings.Char;
using util.Strings;

@:publicFields
class Cursor {
	var line = 0;
	var column = 0;
	var lastCR = false;

	function new() {}

	inline function pos() {
		return new Pos(line, column);
	}

	function append(str: String) {
		for(char in str) appendChar(char);
	}

	function appendChar(char: Char) {
		if(char == Char.CR) {
			line++;
			column = 0;
			lastCR = true;
		} else if(char == Char.LF) {
			if(lastCR) {
				lastCR = false;
			} else {
				line++;
				column = 0;
			}
		} else {
			if(char.isAsciiPrintable() || char == Char.TAB) {
				column++;
			}
			
			lastCR = false;
		}
	}
}