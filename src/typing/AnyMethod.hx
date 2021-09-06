package typing;

import reporting.Diagnostic;
import parsing.ast.Stmt;
import parsing.ast.Ident;
import text.Span;
import typing.Traits;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class AnyMethod implements IErrors {
	final errors: Array<Diagnostic> = [];
	final decl: ITypeDecl;
	final span: Span;
	var hidden: Option<Option<Type>> = None;
	var noInherit: Bool = false;
	var native: Option<Option<Ident>> = None;
	var isAsm: Bool = false;
	final body: Option<Array<Stmt>>;

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		return decl.findType(path, absolute, cache);
	}

	abstract function declName(): String;

	abstract function methodName(): String;

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}