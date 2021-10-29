package typing;

enum abstract BinaryOp(Int) {
	var Plus;
	var Minus;
	var Times;
	var Pow;
	var Div;
	var IntDiv;
	var Mod;
	var IsMod;
	var BitAnd;
	var BitOr;
	var BitXor;
	var Shl;
	var Shr;
	var Eq;
	var Ne;
	var Gt;
	var Ge;
	var Lt;
	var Le;
	var And;
	var Or;
	var Xor;
	var Nor;

	static final SYMBOLS = ["+", "-", "*", "**", "/", "//", "%", "%%", "&", "|", "^", "<<", ">>", "?=", "!=", ">", ">=", "<", "<=", "&&", "||", "^^", "!!"];

	public inline function symbol() {
		return SYMBOLS[this];
	}

	public static function fromInfix(infix: TExpr.Infix): BinaryOp {
		return infix._match(
			at(TExpr.Infix.Plus) => Plus,
			at(TExpr.Infix.Minus) => Minus,
			at(TExpr.Infix.Times) => Times,
			at(TExpr.Infix.Pow) => Pow,
			at(TExpr.Infix.Div) => Div,
			at(TExpr.Infix.IntDiv) => IntDiv,
			at(TExpr.Infix.Mod) => Mod,
			at(TExpr.Infix.IsMod) => IsMod,
			at(TExpr.Infix.BitAnd) => BitAnd,
			at(TExpr.Infix.BitOr) => BitOr,
			at(TExpr.Infix.BitXor) => BitXor,
			at(TExpr.Infix.Shl) => Shl,
			at(TExpr.Infix.Shr) => Shr,
			at(TExpr.Infix.And) => And,
			at(TExpr.Infix.Or) => Or,
			at(TExpr.Infix.Xor) => Xor,
			at(TExpr.Infix.Nor) => Nor,
			at(TExpr.Infix.Eq) => Eq,
			at(TExpr.Infix.Ne) => Ne,
			at(TExpr.Infix.Gt) => Gt,
			at(TExpr.Infix.Ge) => Ge,
			at(TExpr.Infix.Lt) => Lt,
			at(TExpr.Infix.Le) => Le,
			at(Assign(_)) => throw "bad"
		);
	}
}