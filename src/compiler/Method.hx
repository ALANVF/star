package compiler;

class Method extends NamedMethod {
	var args: Array<Param>;
	var hasVarargs: Bool = false;
}