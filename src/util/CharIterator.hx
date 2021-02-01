// A reimplementation of hx.Strings.CharIteraton. Not sure if I'm supposed to
// keep the license here, so I'm leaving it here just in case.

/*
 * Copyright (c) 2016-2020 Vegard IT GmbH (https://vegardit.com) and contributors.
 * SPDX-License-Identifier: Apache-2.0
 */

package util;

import haxe.io.Input;
import hx.strings.internal.Bits;
import hx.strings.internal.RingBuffer;
import hx.strings.internal.TriState;
import hx.strings.AnyAsString;
import hx.strings.Char;

using hx.strings.Strings;

class CharIterator {
	inline public static function fromString(chars: Null<AnyAsString>, prevBufferSize = 0): CharIterator {
		if(chars == null)
			return NullCharIterator.INSTANCE;
		return new StringCharIterator(chars, prevBufferSize);
	}

	inline public static function fromArray(chars: Array<Char>, prevBufferSize = 0): CharIterator {
		if(chars == null)
			return NullCharIterator.INSTANCE;
		return new ArrayCharIterator(chars, prevBufferSize);
	}

	inline public static function fromInput(chars: Null<Input>, prevBufferSize = 0): CharIterator {
		if(chars == null)
			return NullCharIterator.INSTANCE;
		return new InputCharIterator(chars, prevBufferSize);
	}

	inline public static function fromIterator(chars: Null<Iterator<Char>>, prevBufferSize = 0): CharIterator {
		if(chars == null)
			return NullCharIterator.INSTANCE;
		return new IteratorCharIterator(chars, prevBufferSize);
	}

	var index = -1;
	var line = 0;
	var col = 0;
	var currChar = -1;

	final prevBuffer: Null<RingBuffer<CharWithPos>>;
	var prevBufferPrevIdx = -1;
	var prevBufferNextIdx = -1;

	public var current(get, never): Null<Char>;

	inline function get_current()
		return index > -1 ? currChar : null;

	public var pos(get, never): CharPos;

	inline function get_pos()
		return new CharPos(index, line, col);

	function new(prevBufferSize: Int)
		prevBuffer = prevBufferSize > 0 ? new RingBuffer<CharWithPos>(prevBufferSize + 1 /*currChar*/) : null;

	inline public function hasPrev(): Bool
		return prevBufferPrevIdx > -1;

	public function prev(): Char {
		if(!hasPrev())
			throw new Eof();

		final prevChar = prevBuffer[prevBufferPrevIdx];
		currChar = prevChar.char;
		index = prevChar.index;
		line = prevChar.line;
		col = prevChar.col;

		prevBufferNextIdx = prevBufferPrevIdx + 1 < prevBuffer.length ? prevBufferPrevIdx + 1 : -1;
		prevBufferPrevIdx--;
		return currChar;
	}

	inline public function hasNext(): Bool
		return prevBufferNextIdx > -1 ? true : !isEOF();

	public function next(): Char {
		if(prevBufferNextIdx > -1) {
			var prevChar = prevBuffer[prevBufferNextIdx];
			currChar = prevChar.char;
			index = prevChar.index;
			line = prevChar.line;
			col = prevChar.col;
			prevBufferPrevIdx = prevBufferNextIdx - 1;
			prevBufferNextIdx = prevBufferNextIdx + 1 < prevBuffer.length ? prevBufferNextIdx + 1 : -1;
			return currChar;
		}

		if(isEOF())
			throw new Eof();

		if(currChar == Char.LF || currChar < 0) {
			line++;
			col = 0;
		}

		index++;
		col++;
		currChar = getChar();

		if(prevBuffer != null) {
			prevBuffer.add(new CharWithPos(currChar, index, col, line));
			prevBufferPrevIdx = prevBuffer.length - 2;
			prevBufferNextIdx = -1;
		}

		return currChar;
	}

	function getChar(): Char
		throw "Not implemented";

	function isEOF(): Bool
		throw "Not implemented";
}

@:noCompletion
@:noDoc @:dox(hide)
class CharWithPos extends CharPos {
	public final char: Char;

	public function new(char: Char, index: CharIndex, line: Int, col: Int) {
		super(index, line, col);
		this.char = char;
	}
}

@:noDoc @:dox(hide)
private class NullCharIterator extends CharIterator {
	public static final INSTANCE = new NullCharIterator();

	inline function new()
		super(0);

	override function isEOF(): Bool
		return true;
}

@:noDoc @:dox(hide)
private class ArrayCharIterator extends CharIterator {
	final chars: Array<Char>;
	final charsMaxIndex: Int;

	public function new(chars: Array<Char>, prevBufferSize: Int) {
		super(prevBufferSize);
		this.chars = chars;
		charsMaxIndex = chars.length - 1;
	}

	override function isEOF(): Bool
		return index >= charsMaxIndex;

	override function getChar(): Char
		return chars[index];
}

@:noDoc @:dox(hide)
private class IteratorCharIterator extends CharIterator {
	final chars: Iterator<Char>;

	public function new(chars: Iterator<Char>, prevBufferSize: Int) {
		super(prevBufferSize);
		this.chars = chars;
	}

	override function isEOF(): Bool
		return !chars.hasNext();

	override function getChar(): Char
		return chars.next();
}

@:noDoc @:dox(hide)
private class InputCharIterator extends CharIterator {
	var byteIndex = 0;
	final input: Input;
	var currCharIndex = -1;
	var nextChar: Char;
	var nextCharAvailable = TriState.UNKNOWN;

	public function new(chars: Input, prevBufferSize: Int) {
		super(prevBufferSize);
		this.input = chars;
	}

	override function isEOF(): Bool {
		if(nextCharAvailable == UNKNOWN) {
			try {
				nextChar = readUtf8Char();
				nextCharAvailable = TRUE;
			} catch(ex:haxe.io.Eof) {
				nextCharAvailable = FALSE;
			}
		}
		return nextCharAvailable != TRUE;
	}

	override function getChar(): Char {
		if(index != currCharIndex) {
			currCharIndex = index;
			nextCharAvailable = UNKNOWN;
			return nextChar;
		}
		return currChar;
	}

	inline function readUtf8Char(): Char {
		var byte1 = input.readByte();
		byteIndex++;
		if(byte1 <= 127)
			return byte1;

		/*
		 * determine the number of bytes composing this UTF char
		 * and clear the control bits from the first byte.
		 */
		byte1 = Bits.clearBit(byte1, 8);
		byte1 = Bits.clearBit(byte1, 7);
		var totalBytes = 2;

		final isBit6Set = Bits.getBit(byte1, 6);
		var isBit5Set = false;
		if(isBit6Set) {
			byte1 = Bits.clearBit(byte1, 6);
			totalBytes++;

			isBit5Set = Bits.getBit(byte1, 5);
			if(isBit5Set) {
				byte1 = Bits.clearBit(byte1, 5);
				totalBytes++;

				if(Bits.getBit(byte1, 4))
					throw 'Valid UTF-8 byte expected at position [$byteIndex] but found byte with value [$byte1]!';
			}
		}

		var result: Int = (byte1 << 6 * (totalBytes - 1));

		/*
		 * read the second byte
		 */
		final byte2 = readUtf8MultiSequenceByte();
		result += (byte2 << 6 * (totalBytes - 2));

		/*
		 * read the third byte
		 */
		if(isBit6Set) {
			final byte3 = readUtf8MultiSequenceByte();
			result += (byte3 << 6 * (totalBytes - 3));

			/*
			 * read the fourth byte
			 */
			if(isBit5Set) {
				final byte4 = readUtf8MultiSequenceByte();
				result += (byte4 << 6 * (totalBytes - 4));
			}
		}

		// UTF8-BOM marker http://unicode.org/faq/utf_bom.html#bom4
		if(index == 0 && result == 65279)
			return readUtf8Char();

		return result;
	}

	inline function readUtf8MultiSequenceByte(): Int {
		final byte = input.readByte();
		byteIndex++;

		if(!Bits.getBit(byte, 8))
			throw 'Valid UTF-8 multi-sequence byte expected at position [$byteIndex] but found byte with value [$byte]!';

		if(Bits.getBit(byte, 7))
			throw 'Valid UTF-8 multi-sequence byte expected at position [$byteIndex] but found byte with value [$byte]!';

		return Bits.clearBit(byte, 8);
	}
}

@:noDoc @:dox(hide)
class StringCharIterator extends CharIterator {
	final chars: String;
	final charsMaxIndex: Int;

	public function new(chars: String, prevBufferSize: Int) {
		super(prevBufferSize);
		this.chars = chars;
		charsMaxIndex = chars.length8() - 1;
	}

	override function isEOF(): Bool
		return index >= charsMaxIndex;

	override function getChar(): Char
		return @:privateAccess Strings._charCodeAt8Unsafe(chars, index);
}
