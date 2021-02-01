package lexing;

import hl.Bytes;
import hx.strings.Char;

using util.Strings;

@:struct
@:publicFields
private class _Charset {
	var bytes: Bytes;
	var size: Int;

	function new(size) {
		this.bytes = new Bytes(size);
		this.bytes.fill(0, size, 0);
		this.size = size;
	}

	private static inline function toByte(ord) {
		return 1 << (7 - (ord & 7));
	}
}

@:forward(bytes, size)
abstract Charset(_Charset) {
	inline function new(size) {
		this = new _Charset(size);
	}

	public static inline function from(str: String) {
		return _of(str.iterator());
	}

	public static inline function of(chars: Iterable<Char>) {
		return _of(chars.iterator());
	}

	static function _of(chars: Iterator<Char>) {
		final ords = [for(char in chars) char.toInt()];
		final maxBit = ords.max();
		final out = new Charset((maxBit >> 3) + 1);

		for(ord in ords) {
			out.bytes[ord >> 3] += toByte(ord);
		}

		return out;
	}

	static inline function toByte(ord) {
		return 1 << (7 - (ord & 7));
	}

	@:arrayAccess
	function has(char: Char) {
		final ord = char.toInt();
		final i = ord >> 3;
		
		return i < this.size && (toByte(ord) & this.bytes[i]) != 0;
	}

	@:arrayAccess
	function set(char: Char, status: Bool) {
		final ord = char.toInt();
		final i = ord >> 3;
		final byte = toByte(ord);
		
		if(!status) {
			if(i < this.size) {
				this.bytes[i] &= ~byte;
			}
		} else if(i >= this.size) {
			final extraSize = i - this.size;
			final newBytes = new Bytes(this.size + extraSize + 1);

			newBytes.blit(0, this.bytes, 0, this.size);
			newBytes.fill(this.size, extraSize, 0);
			newBytes[i] = byte;

			this.bytes = newBytes;
		} else {
			this.bytes[i] |= byte;
		}
	}

	@:op(A | B)
	function union(other: Charset) {
		final out = new Charset(util.Math.max(this.size, other.size));

		out.bytes.blit(0, this.bytes, 0, this.size);

		for(i in 0...(other.size << 3)) {
			out[i] = out[i] || other[i];
		}

		return out;
	}

	@:commutative @:op(A | B)
	function unionChar(other: Char) {
		final out = new Charset(this.size);

		out.bytes.blit(0, this.bytes, 0, this.size);
		out[other] = true;

		return out;
	}
}