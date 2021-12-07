package parsing.ast;

@:using(parsing.ast.Suffix)
enum Suffix {
	SIncr;
	SDecr;
	STruthy;
}

function symbol(self: Suffix) return self._match(
	at(SIncr) => "++",
	at(SDecr) => "--",
	at(STruthy) => "?"
);