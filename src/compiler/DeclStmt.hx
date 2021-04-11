package compiler;

enum DeclStmt {
	DTypeDecl(decl: TypeDecl);
	DNamespace(ns: Namespace);
	DUsing(kind: Option<String>, type: Type);
	DUsingExpr(expr: Expr);
	DIncludeLib(path: String);
	DIncludeFile(path: String);
	DMember(m: Member);
	DMethod(m: AnyMethod);
}