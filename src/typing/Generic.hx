package typing;

import parsing.ast.Ident;

@:build(util.Auto.build())
class Generic {
	var lookup: ILookupType;
	var name: Ident;
	var params: Option<Array<Type>>;
	var parents: Option<Array<Type>>;
	var rule: Option<GenericRule>;
	//var body: Option<DeclBody>;
}