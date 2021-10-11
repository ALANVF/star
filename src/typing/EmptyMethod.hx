package typing;

import reporting.Diagnostic;
import parsing.ast.Stmt;
import text.Span;
import typing.Traits;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class EmptyMethod implements IErrors {
	final errors: Array<Diagnostic> = [];
	final decl: ITypeDecl;
	final span: Span;
	final body: Array<Stmt>;

	@ignore var typedBody: Null<Array<TStmt>> = null;

	abstract function declName(): String;

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}