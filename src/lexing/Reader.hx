package lexing;

import hx.strings.Char;
import util.CharIterator;

using hx.strings.Strings;

class Reader extends CharIterator.StringCharIterator {
	public function peek() {
		return if(hasNext()) {
			@:privateAccess inline chars._charCodeAt8Unsafe(index+1);
		} else {
			null;
		}
	}

	/*public function peekN(size) {
		return if(index + size < charsMaxIndex) {
			chars.substr8(index, size);
		} else {
			null;
		}
	}

	public function nextN(size) {
		if(index + size < charsMaxIndex) {
			final result = chars.substr8(index, size);
			for(_ in 0...size) next();
			return result;
		} else {
			throw new Eof();
		}
	}*/

	public function skip() {
		if(prevBufferNextIdx > -1) {
			final prevChar = prevBuffer[prevBufferNextIdx];
			currChar = prevChar.char;
			index = prevChar.index;
			line = prevChar.line;
			col = prevChar.col;
			prevBufferPrevIdx = prevBufferNextIdx - 1;
			prevBufferNextIdx = prevBufferNextIdx + 1 < prevBuffer.length ? prevBufferNextIdx + 1 : -1;
		} else if(isEOF()) {
			throw new Eof();
		} else {
			if (currChar == Char.LF || currChar < 0) {
				line++;
				col = 0;
			}

			index++;
			col++;
			currChar = inline getChar();

			if(prevBuffer != null) {
				prevBuffer.add(new util.CharIterator.CharWithPos(currChar, index, col, line));
				prevBufferPrevIdx = prevBuffer.length - 2;
				prevBufferNextIdx = -1;
			}
		}
	}

	public function advance() {
		final char = inline getChar();
		inline skip();
		return char;
	}

	public inline function eat(char: Char) {
		return if(inline getChar() == char) {
			skip();
			true;
		} else {
			false;
		}
	}
}