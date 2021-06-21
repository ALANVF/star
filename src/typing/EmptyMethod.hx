package typing;

import parsing.ast.Stmt;
import text.Span;
import typing.Traits;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class EmptyMethod {
	final decl: ITypeDecl;
	final span: Span;
	final body: Array<Stmt>;
}