package typing;

import reporting.Diagnostic;
import parsing.ast.Stmt;
import parsing.ast.Ident;
import text.Span;
import typing.Traits;

@:build(util.Auto.build())
@:autoBuild(util.Auto.build())
abstract class AnyMethod implements ITypeLookupDecl {
	final errors: Array<Diagnostic> = [];
	final decl: AnyTypeDecl;
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

	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		return decl.findType(path, Start, from, depth, cache);
	}

	abstract function methodName(): String;

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}


	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return decl.findCategory(ctx, cat, forType, from, cache);
	}
}