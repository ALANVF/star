package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build())
class Generic implements ITypeDecl {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var parents: Option<Array<Type>>;
	var rule: Option<GenericRule>;
	//var body: Option<DeclBody>;

	inline function declName() {
		return "generic type";
	}

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}
}