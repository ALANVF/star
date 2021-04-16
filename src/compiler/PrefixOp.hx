package compiler;

@:using(compiler.PrefixOp.PrefixOpTools)
enum PrefixOp {
	PPos;
	PNeg;
	PCompl;
	PNot;
	PAddr;
	PDeref;
	PIncr;
	PDecr;
	PScope;
	PUserDefined(name: String);
}


@:publicFields
class PrefixOpTools {
	static function form(prefix: PrefixOp) {
		return switch prefix {
			case PPos: "+";
			case PNeg: "-";
			case PCompl: "~";
			case PNot: "!";
			case PAddr: "&";
			case PDeref: "*";
			case PIncr: "++";
			case PDecr: "--";
			case PScope: "::";
			case PUserDefined(name): name;
		}
	}
}