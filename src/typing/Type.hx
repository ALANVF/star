package typing;

import typing.ILookupType;
import text.Span;

enum TypeKind {
	TPath(path: TypePath, source: ILookupType);
	TConcrete(decl: TypeDecl);
	TThis(span: Option<Span>, source: ILookupType);
	TErased(span: Option<Span>);
}

@:build(util.Auto.build())
class Type implements ILookupType {
	var t: TypeKind;

	function lookupTypePath(path: TypePath, isBase = false): Option<Type> {
		/*Util.match(path,
			at([Blank(_, _), ..._]) => throw "NYI!",
			at([Named(span, "This", )]) => 
		);*/
		throw "NYI!";
	}
}