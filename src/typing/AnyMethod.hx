package typing;

import parsing.ast.Stmt;
import parsing.ast.Ident;
import text.Span;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class AnyMethod implements IAnyMethod {
	final decl: ITypeDecl;
	final span: Span;
	var hidden: Option<Option<Type>> = None;
	var noInherit: Bool = false;
	var isNative: Option<Ident> = None;
	var isAsm: Bool = false;
	final body: Option<Array<Stmt>>;
}