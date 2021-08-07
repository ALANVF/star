package compiler.nim;

@:using(compiler.nim.Infix.InfixTools)
enum Infix {
	IAdd;
	ISub;
	IMult;
	IDiv; // div
	IIntDiv; // /
	IMod; // mod
	IIntMod; // %
	IPow;
	IShl;
	IShr;
	IConcat;
	IRange;
	IRangeExcl;
	IRangeEnd;
	IEq;
	INe;
	IGt;
	IGe;
	ILt;
	ILe;
	IUnsigned(op: Infix);
	//IIn;
	//INotIn;
	//IIs;
	//IIsNot;
	//IOf;
	//IAs;
	IAnd;
	IOr;
	IXor;
	//IAssign;
	IAssignOp(op: Infix);
	IOp(op: String);
}

@:publicFields
class InfixTools {
	static function toNim(self: Infix) return switch self {
		case IAdd: "+";
		case ISub: "-";
		case IMult: "*";
		case IDiv: "/";
		case IIntDiv: "div";
		case IMod: "%";
		case IIntMod: "mod";
		case IPow: "^";
		case IShl: "shl";
		case IShr: "shr";
		case IConcat: "&";
		case IRange: "..";
		case IRangeExcl: "..<";
		case IRangeEnd: "..^";
		case IEq: "==";
		case INe: "!=";
		case IGt: ">";
		case IGe: ">=";
		case ILt: "<";
		case ILe: "<=";
		case IUnsigned(op): op.toNim() + "%";
		case IAnd: "and";
		case IOr: "or";
		case IXor: "xor";
		case IAssignOp(op): op.toNim() + "=";
		case IOp(op): op;
	}
}