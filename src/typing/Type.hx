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

	function simpleName() return switch t {
		case TPath(path, _): path.simpleName();
		case TConcrete({name: {name: name}, params: None}): name;
		case TConcrete({name: {name: name}, params: Some(params)}): '$name[${params.map(_ -> "...").join(", ")}]';
		case TThis(_, _): "This";
		case TErased(_): "_";
	}
}