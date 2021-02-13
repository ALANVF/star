package typing;

import text.Span;
import parsing.ast.Ident;

@:build(util.Auto.build())
class Generic implements ITypeDecl {
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var params: Option<Array<Type>>;
	var parents: Option<Array<Type>>;
	var rule: Option<GenericRule>;
	//var body: Option<DeclBody>;

	function makeTypePath(path) {
		return new Type(TPath(path, this));
	}
}