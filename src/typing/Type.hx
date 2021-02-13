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

	function new(t: TypeKind) {
		this.t = t;
	}

	function makeTypePath(path: TypePath) {
		return new Type(TPath(path, this));
	}
}