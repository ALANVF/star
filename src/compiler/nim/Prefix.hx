package compiler.nim;

@:using(compiler.nim.Prefix.PrefixTools)
enum Prefix {
	PDollar;
	PAt;
	PNeg;
	PNot;
	PEnd;
	POp(op: String);
}

@:publicFields
class PrefixTools {
	static function toNim(self: Prefix) return switch self {
		case PDollar: "$";
		case PAt: "@";
		case PNeg: "-";
		case PNot: "not";
		case PEnd: "^";
		case POp(op): op;
	}
}