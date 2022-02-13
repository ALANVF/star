package parsing.ast;

import errors.Error;
import parsing.ast.decls.Decl;

enum ScriptDecl {
	SDecl(decl: Decl);
	SStmt(stmt: Stmt);
}

enum Program {
	Modular(errors: Array<Error>, decls: Array<Decl>);
	Script(errors: Array<Error>, decls: Array<ScriptDecl>);
}