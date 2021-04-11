package compiler;

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