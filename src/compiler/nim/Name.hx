package compiler.nim;

@:using(compiler.nim.Name.NameTools)
enum Name {
	NName(n: String);
	NSym(s: String);
}

@:publicFields
class NameTools {
	static function toNim(self: Name) return self._match(
		at(NName(n)) => n,
		at(NSym(s)) => '`$s`'
	);
	
	static function toExpr(self: Name): Expr return self._match(
		at(NName(n)) => EName(n),
		at(NSym(s)) => ESym(s)
	);
}