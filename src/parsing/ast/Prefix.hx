package parsing.ast;

@:using(parsing.ast.Prefix)
enum Prefix {
	PIncr;
	PDecr;
	PNeg;
	PNot;
	PCompl;
	PSpread;
}

function symbol(self: Prefix) return self._match(
	at(PIncr) => "++",
	at(PDecr) => "--",
	at(PNeg) => "-",
	at(PNot) => "!",
	at(PCompl) => "~",
	at(PSpread) => "..."
);