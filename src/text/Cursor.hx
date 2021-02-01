package text;

import hx.strings.Char;
using hx.strings.Strings;
using util.Strings;

@:publicFields
class Cursor {
	var pos: Pos = new Pos(0, 0);
	var lastCR = false;

	function new(?pos) this.pos = pos == null ? new Pos(0, 0) : pos;

	function append(str: String) {
		for(char in str) appendChar(char);
	}

	function appendChar(char: Char) {
		if(char == Char.CR) {
			pos = pos.newline();
			lastCR = true;
		} else if(char == Char.LF) {
			if(!lastCR) pos = pos.newline();
			lastCR = false;
		} else {
			if(char.isAsciiPrintable() || char == Char.TAB) {
				pos = pos.advance();
			}
			
			lastCR = false;
		}
	}
}