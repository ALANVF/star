package lexing;

import hx.strings.Char;
import text.Cursor;

using hx.strings.Strings;

@:publicFields
class Reader {
	final cursor = new Cursor();
	private final input: String;
	private final length: Int;
	private var offset = 0;

	function new(input: String) {
		this.input = input;
		length = input.length8();
	}

	inline function hasNext() {
		return offset != length;
	}

	inline function hasNextAt(index: Int) {
		return offset + index < length;
	}

	inline function unsafePeek() {
		return @:privateAccess input._charCodeAt8Unsafe(offset);
	}

	inline function unsafePeekAt(index: Int) {
		return @:privateAccess input._charCodeAt8Unsafe(offset + index);
	}

	extern inline overload function peek() return peekNext();
	private function peekNext() {
		return if(hasNext()) {
			unsafePeek();
		} else {
			null;
		}
	}

	extern inline overload function peek(char: Char) return peekChar(char);
	private function peekChar(char: Char) {
		return hasNext() && char == unsafePeek();
	}

	extern inline overload function peek(str: String) return peekString(str);
	private function peekString(str: String) {
		return hasNextAt(str.length) && {
			for(i in 0...str.length) {
				if(@:privateAccess input._charCodeAt8Unsafe(i) != @:privateAccess str._charCodeAt8Unsafe(i)) {
					return false;
				}
			}

			true;
		};
	}
	
	extern inline overload function peek(charset: Charset) return peekCharset(charset);
	private function peekCharset(charset: Charset) {
		return hasNext() && charset[unsafePeek()];
	}


	extern inline overload function peekAt(index: Int, char: Char) return peekAtChar(index, char);
	private function peekAtChar(index: Int, char: Char) {
		return hasNextAt(index) && char == unsafePeekAt(index);
	}

	extern inline overload function peekAt(index: Int, charset: Charset) return peekAtCharset(index, charset);
	private function peekAtCharset(index: Int, charset: Charset) {
		return hasNextAt(index) && charset[unsafePeekAt(index)];
	}

	
	extern inline overload function peekNot(char: Char) return peekNotChar(char);
	private function peekNotChar(char: Char) {
		return hasNext() && char != unsafePeek();
	}
	
	extern inline overload function peekNot(charset: Charset) return peekNotCharset(charset);
	private function peekNotCharset(charset: Charset) {
		return hasNext() && !charset[unsafePeek()];
	}


	function peekNotAt(index: Int, charset: Charset) {
		return hasNextAt(index) && !charset[unsafePeekAt(index)];
	}


	extern inline overload function eat() return eatNext();
	private function eatNext() {
		final char = unsafePeek();

		offset++;
		cursor.appendChar(char);

		return char;
	}
	
	extern inline overload function eat(char: Char) return eatChar(char);
	private function eatChar(char: Char) {
		return if(peek(char)) {
			offset++;
			cursor.appendChar(char);
			true;
		} else {
			false;
		}
	}

	extern inline overload function eat(str: String) return eatString(str);
	private function eatString(str: String) {
		return if(peek(str)) {
			offset += str.length;
			cursor.append(str);
			true;
		} else {
			false;
		}
	}

	function next() {
		if(hasNext()) {
			cursor.appendChar(unsafePeek());
			offset++;
		}
	}
}