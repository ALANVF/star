package typing;

import typing.ILookupType;
import text.Span;

using hx.strings.Strings;

enum TypeKind {
	TPath(path: TypePath, source: ILookupType);
	TConcrete(decl: TypeDecl);
	TThis(span: Option<Span>, source: ITypeDecl);
	TErased(span: Option<Span>);
	TMulti(types: Array<Type>);
	TApplied(type: Type, params: Array<Type>);
	TGeneric(generic: Generic);
}

@:build(util.Auto.build())
class Type {
	var t: TypeKind;

	function new(t: TypeKind) {
		this.t = t;
	}

	function makeTypePath(path: TypePath) {
		return new Type(TPath(path, this));
	}

	function simpleName(): String return switch t {
		case TPath(path, _): path.simpleName();
		case TConcrete({name: {name: name}, params: None})
			| TGeneric({name: {name: name}, params: None}): name;
		case TConcrete({name: {name: name}, params: Some(params)})
			| TGeneric({name: {name: name}, params: Some(params)}): '$name[${params.map(_ -> "...").join(", ")}]';
		case TThis(_, _): "This";
		case TErased(_): "_";
		case TMulti(types): types[0].simpleName();
		case TApplied(type, params): // Probably bad but eh
			final name = type.simpleName();
			(if(name.endsWith("]")) {
				name.removeAfterLast("[");
			} else {
				name;
			}) + "[" + params.map(p -> p.simpleName()).join(", ") + "]";
	}
}