package compiler.nim;

@:using(compiler.nim.Pragmas.PragmasTools)
typedef Pragmas = Array<Pragma>;

@:publicFields
class PragmasTools {
	static function toNim(self: Pragmas) return "{." + self.joinMap(", ", p -> p.toNim()) + ".}";
}