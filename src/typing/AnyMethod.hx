package typing;

import reporting.Diagnostic;
import parsing.ast.Stmt;
import parsing.ast.Ident;
import text.Span;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class AnyMethod {
	final errors: Array<Diagnostic> = [];
	final decl: ITypeDecl;
	final span: Span;
	var hidden: Option<Option<Type>> = None;
	var noInherit: Bool = false;
	var native: Option<Option<Ident>> = None;
	var isAsm: Bool = false;
	final body: Option<Array<Stmt>>;

	abstract function declName(): String;

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}