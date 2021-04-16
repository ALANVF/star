package compiler;

@:using(compiler.InfixOp.InfixOpTools)
enum InfixOp {
	IAssign(op: Option<InfixOp>);
	IAdd;
	ISub;
	IMult;
	IDiv;
	IMod;
	IBitAnd;
	IBitOr;
	IBitXor;
	IShl;
	IShr;
	ICmp;
	IEq;
	INe;
	IGt;
	IGe;
	ILt;
	ILe;
	IAnd;
	IOr;
	IComma;
}


@:publicFields
class InfixOpTools {
	static function form(infix: InfixOp) {
		return switch infix {
			case IAssign(None): "=";
			case IAssign(Some(op)): op.form() + "=";
			case IAdd: "+";
			case ISub: "-";
			case IMult: "*";
			case IDiv: "/";
			case IMod: "%";
			case IBitAnd: "&";
			case IBitOr: "|";
			case IBitXor: "^";
			case IShl: "<<";
			case IShr: ">>";
			case ICmp: "<=>";
			case IEq: "==";
			case INe: "!=";
			case IGt: ">";
			case IGe: ">=";
			case ILt: "<";
			case ILe: "<=";
			case IAnd: "&&";
			case IOr: "||";
			case IComma: ",";
		}
	}
}