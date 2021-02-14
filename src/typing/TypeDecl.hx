package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build({init: false}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl
	implements IErrors
	implements ITypeDecl
{
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final generics: Array<Generic>;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	
	abstract function declName(): String;

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}

	function hasErrors() {
		return errors.length != 0;
	}

	function allErrors() {
		return errors;
	}
}