package parsing.ast;

import reporting.Diagnostic;
import parsing.ast.decls.Decl;

enum ScriptDecl {
	SDecl(decl: Decl);
	SStmt(stmt: Stmt);
}

enum Program {
	Modular(errors: Array<Diagnostic>, decls: Array<Decl>);
	Script(errors: Array<Diagnostic>, decls: Array<ScriptDecl>);
}