package compiler;

@:using(compiler.SuffixOp.SuffixOpTools)
enum SuffixOp {
	SIncr;
	SDecr;
	SSpread;
}


@:publicFields
class SuffixOpTools {
	static function form(suffix: SuffixOp) {
		return switch suffix {
			case SIncr: "++";
			case SDecr: "--";
			case SSpread: "...";
		}
	}
}