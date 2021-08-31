package text;

@:publicFields
private class _Pos {
	final line: Int;
	final column: Int;

	public inline function new(line: Int, column: Int) {
		this.line = line;
		this.column = column;
	}

	public function toString() return 'line ${line + 1}, column ${column + 1}';
}

@:forward(line, column, toString)
abstract Pos(_Pos) {
	public inline function new(line, column) this = new _Pos(line, column);

	public function compare(other: Pos) return switch this.line - other.line {
		case 0: this.column - other.column;
		case cmp: cmp;
	}

	@:op(A == B) inline function eq(other: Pos) return compare(other) == 0;
	@:op(A != B) inline function ne(other: Pos) return compare(other) != 0;
	@:op(A > B) inline function gt(other: Pos) return compare(other) > 0;
	@:op(A >= B) inline function ge(other: Pos) return compare(other) >= 0;
	@:op(A < B) inline function lt(other: Pos) return compare(other) < 0;
	@:op(A <= B) inline function le(other: Pos) return compare(other) <= 0;

	public inline function advance(amount = 1) return new Pos(this.line, this.column + amount);

	public inline function newline() return new Pos(this.line + 1, 0);
}