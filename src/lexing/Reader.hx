package lexing;

import hx.strings.Char;
import text.Cursor;

@:publicFields
class Reader {
	final cursor = new Cursor();
	private final input: String;
	private final length: Int;
	var offset = 0;

	inline function new(input: String) {
		this.input = input;
		length = input.length8();
	}

	inline function hasNext() {
		return offset < length;
	}

	inline function hasNextAt(index: Int) {
		return offset + index < length;
	}

	extern inline overload function unsafePeek() return unsafePeekNext();
	private inline function unsafePeekNext() {
		return @:privateAccess input._charCodeAt8Unsafe(offset);
	}

	extern inline overload function unsafePeek(char: Char) return unsafePeekChar(char);
	private inline function unsafePeekChar(char: Char) {
		return @:privateAccess input._charCodeAt8Unsafe(offset) == char;
	}

	inline function unsafePeekAt(index: Int) {
		return @:privateAccess input._charCodeAt8Unsafe(offset + index);
	}

	extern inline overload function peek() return peekNext();
	private inline function peekNext() {
		return if(hasNext()) {
			unsafePeek();
		} else {
			null;
		}
	}

	extern inline overload function peek(char: Char) return peekChar(char);
	private inline function peekChar(char: Char) {
		return hasNext() && char == unsafePeek();
	}

	extern inline overload function peek(str: String) return peekString(str);
	private function peekString(str: String) {
		return hasNextAt(str.length) && {
			for(i in 0...str.length) {
				if(@:privateAccess input._charCodeAt8Unsafe(offset + i) != @:privateAccess str._charCodeAt8Unsafe(i)) {
					return false;
				}
			}

			true;
		};
	}
	
	extern inline overload function peek(charset: Charset) return peekCharset(charset);
	private inline function peekCharset(charset: Charset) {
		return hasNext() && charset[unsafePeek()];
	}


	extern inline overload function peekAt(index: Int) return peekAtNext(index);
	private inline function peekAtNext(index: Int) {
		return hasNextAt(index) ? unsafePeekAt(index) : null;
	}
	
	extern inline overload function peekAt(index: Int, char: Char) return peekAtChar(index, char);
	private inline function peekAtChar(index: Int, char: Char) {
		return hasNextAt(index) && char == unsafePeekAt(index);
	}

	extern inline overload function peekAt(index: Int, charset: Charset) return peekAtCharset(index, charset);
	private inline function peekAtCharset(index: Int, charset: Charset) {
		return hasNextAt(index) && charset[unsafePeekAt(index)];
	}

	
	extern inline overload function peekNot(char: Char) return peekNotChar(char);
	private inline function peekNotChar(char: Char) {
		return hasNext() && char != unsafePeek();
	}
	
	extern inline overload function peekNot(charset: Charset) return peekNotCharset(charset);
	private inline function peekNotCharset(charset: Charset) {
		return hasNext() && !charset[unsafePeek()];
	}


	inline function peekNotAt(index: Int, charset: Charset) {
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

	function safeNext() {
		if(hasNext()) {
			cursor.appendChar(unsafePeek());
			offset++;
		}
	}

	inline function next() {
		cursor.appendChar(unsafePeek());
		offset++;
	}
	
	extern inline overload function substring(startIndex) {
		return input.substring(startIndex, offset);
	}
	extern inline overload function substring(startIndex, endIndex) {
		return input.substring(startIndex, endIndex);
	}


	inline function peekDigit() return hasNext() && unsafePeek()._match(
		at('0'.code ... '9'.code) => true,
		_ => false
	);

	inline function peekLowerU() return hasNext() && unsafePeek()._match(
		at(('a'.code ... 'z'.code)
		 | '_'.code
		) => true,
		_ => false
	);

	inline function peekAlphaU() return hasNext() && unsafePeek()._match(
		at(('a'.code ... 'z'.code)
		 | ('A'.code ... 'Z'.code)
		 | '_'.code
		) => true,
		_ => false
	);

	inline function peekAlnum() return hasNext() && unsafePeek()._match(
		at(('a'.code ... 'z'.code)
		 | ('A'.code ... 'Z'.code)
		 | ('0'.code ... '9'.code)
		 | '_'.code
		) => true,
		_ => false
	);
	
	inline function peekAlnumQ() return hasNext() && unsafePeek()._match(
		at(('a'.code ... 'z'.code)
		 | ('A'.code ... 'Z'.code)
		 | ('0'.code ... '9'.code)
		 | '_'.code
		 | "'".code
		) => true,
		_ => false
	);
}