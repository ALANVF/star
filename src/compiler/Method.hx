package compiler;

class Method extends NamedMethod {
	var args: Array<Param>;
	var ret: Type;
	var hasVarargs: Bool = false;
}