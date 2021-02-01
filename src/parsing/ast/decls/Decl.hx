package parsing.ast.decls;

/*
interface AllowsMember {}
interface AllowsCase {}
// ...
interface AllowsModule {}
interface AllowsClass {}
interface AllowsProtocol {}
interface AllowsCategory {}
// ...
*/


enum Decl/*<T>*/ {
	DMember(m: Member);//: Decl<AllowsMember>;
	DCase(c: Case);//: Decl<AllowsCase>;

	DModule(m: Module);//: Decl<AllowsModule>;
	DClass(c: Class);//: Decl<AllowsClass>;
	DProtocol(p: Protocol);//: Decl<AllowsProtocol>;
	DCategory(c: Category);//: Decl<AllowsCategory>;
	DKind(k: Kind);
	DAlias(a: Alias);

	DMethod(m: Method);
	DInit(i: Init);
	DDefaultInit(i: BaseMethod);
	DOperator(o: Operator);
	DDeinit(d: BaseMethod);

	DUse(u: Use);
}