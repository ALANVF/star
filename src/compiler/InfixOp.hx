package compiler;

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