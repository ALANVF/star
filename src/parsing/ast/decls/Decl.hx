package parsing.ast.decls;

enum Decl {
	DMember(m: Member);
	DCase(c: Case);

	DModule(m: Module);
	DClass(c: Class);
	DProtocol(p: Protocol);
	DCategory(c: Category);
	DKind(k: Kind);
	DAlias(a: Alias);

	DMethod(m: Method);
	DInit(i: Init);
	DDefaultInit(i: BaseMethod);
	DOperator(o: Operator);
	DDeinit(d: BaseMethod);

	DUse(u: Use);
}