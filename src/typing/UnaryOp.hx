package typing;

enum abstract UnaryOp(Int) {
	var Incr;
	var Decr;
	var Neg;
	var Not;
	var Compl;
	var Truthy;

	static final SYMBOLS = ["++", "--", "-", "!", "~", "?"];

	public inline function symbol() {
		return SYMBOLS[this];
	}
}