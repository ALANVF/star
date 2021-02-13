package typing;

import parsing.ast.Stmt;
import parsing.ast.Ident;
import text.Span;

interface IAnyMethod {
	final decl: ITypeDecl;
	final span: Span;
	var hidden: Option<Option<Type>>;
	var noInherit: Bool;
	var isNative: Option<Ident>;
	var isAsm: Bool;
	final body: Option<Array<Stmt>>;
}