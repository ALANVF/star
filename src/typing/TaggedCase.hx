package typing;

import parsing.ast.Message;
import parsing.ast.Stmt;
import reporting.Severity;
import errors.Error;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:publicFields
@:structInit
abstract class TaggedCase implements IDecl {
	final errors: Array<Error> = [];
	//final decl: ITaggedCases;
	var decl: AnyTypeDecl;
	var span: Span;
	var assoc: Null<Message<parsing.ast.Type>>;
	var init: Null<Array<Stmt>> = null;

	var typedAssoc: Null<typing.Message<Type>> = null;
	var typedInit: Null<TStmts> = null;

	static function fromAST<T: AnyTypeDecl/*, ITaggedCases*/>(decl: T, ast: parsing.ast.decls.Case): TaggedCase {
		switch ast.kind {
			case Tagged({of: Single(name)}, assoc):
				return ({
					decl: decl,
					span: ast.span,
					name: name,
					assoc: assoc,
					init: ast.init?.stmts
				}:SingleTaggedCase);
			
			case Tagged({of: Multi(params)}, assoc):
				return ({
					decl: decl,
					span: ast.span,
					params: params.map(p -> MultiParam.fromUntyped(decl, p)),
					assoc: assoc,
					init: ast.init?.stmts
				}:MultiTaggedCase);
			
			default: throw "Error!";
		}
	}

	function declName() {
		return "tagged case";
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}