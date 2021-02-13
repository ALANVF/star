package typing;

import parsing.ast.Stmt;
import parsing.ast.Ident;

interface IAnyMethod extends IDecl {
	final decl: ITypeDecl;
	var hidden: Option<Option<Type>>;
	var noInherit: Bool;
	var native: Option<Option<Ident>>;
	var isAsm: Bool;
	final body: Option<Array<Stmt>>;
}