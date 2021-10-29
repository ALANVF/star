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

	@ignore var typedBody: Null<Array<TStmt>> = null;

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		return decl.findType(path, Start, from, depth, cache);
	}

	abstract function declName(): String;

	abstract function methodName(): String;

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}


	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return decl.findCategory(cat, forType, from, cache);
	}
}