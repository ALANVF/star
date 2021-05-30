category Expr for Char {
	on [escape] (Str) {
		match this {
			at #"\"" || #"\\" => return "\\\(this)"
			at #"\n" => return "\\n"
			at #"\r" => return "\\r"
			at #"\t" => return "\\t"
			at (
				|| 0 <= _ <= 8
				|| 11 <= _ <= 12
				|| 14 <= _ <= 31
			) => return "\\x" + this[Int][Power toBase: 16 minDigits: 2]
			else => return this[Str]
		}
	}
}