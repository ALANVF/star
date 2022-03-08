package text;

abstract Pos(UInt) {
	public var line(get, never): hl.UI16;
	inline function get_line() {
		return ((this >> 16) : hl.UI16);
	}
	
	public var column(get, never): hl.UI16;
	inline function get_column() {
		return ((this & (0xffff : UInt)) : hl.UI16);
	}
	
	public inline function new(line: hl.UI16, column: hl.UI16) {
		this = ((line : UInt) << 16) | (column : UInt);
	}
	
	private inline function value(): UInt { return this; }

	public inline function compare(other: Pos) {
		return this - other.value();
	}

	@:op(A == B) inline function eq(other: Pos) return this == other.value();
	@:op(A != B) inline function ne(other: Pos) return this != other.value();
	@:op(A > B) inline function gt(other: Pos) return this > other.value();
	@:op(A >= B) inline function ge(other: Pos) return this >= other.value();
	@:op(A < B) inline function lt(other: Pos) return this < other.value();
	@:op(A <= B) inline function le(other: Pos) return this <= other.value();

	public inline function advance(amount: hl.UI16 = 1) {
		return (untyped (this + (amount : UInt)) : Pos);
	}

	public inline function newline() {
		return (untyped (this + (0x00010000 : UInt)) : Pos);
	}

	public function toString() {
		return 'line ${line + 1}, column ${column + 1}';
	}
}