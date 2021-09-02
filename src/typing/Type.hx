package typing;

// THINGS:
// https://github.com/nim-lang/Nim/blob/devel/compiler/semtypinst.nim
// https://github.com/nim-lang/Nim/blob/devel/compiler/types.nim
// https://github.com/nim-lang/Nim/blob/devel/compiler/modulegraphs.nim
// https://github.com/nim-lang/Nim/blob/devel/compiler/sigmatch.nim#L287
// https://github.com/nim-lang/Nim/blob/devel/compiler/semcall.nim#L76
// https://github.com/nim-lang/Nim/blob/devel/compiler/sigmatch.nim#L1980

import text.Span;
import typing.Traits;

// probably need to add something for hkts?
enum TypeKind {
	TPath(path: TypePath, source: ILookupType);
	TConcrete(decl: TypeDecl);
	TThis(span: Option<Span>, source: ITypeDecl);
	TErased(span: Span);
	TMulti(types: Array<Type>);
	TApplied(type: Type, params: Array<Type>);
	TGeneric(typevar: TypeVar);
	TModular(type: Type, unit: Unit);
}

@:build(util.Auto.build())
class Type {
	var t: TypeKind;

	function new(t: TypeKind) {
		this.t = t;
	}
	
	static function getFullPath(lookup: ILookupType): Option<String> {
		return lookup._match(
			at(file is File) => {
				if(file.dir is Unit) {
					var dir = file.dir;
					var names = Nil;
		
					while(dir is Unit) {
						final unit = cast(dir, Unit);
		
						names = names.prepend(unit.name);
						dir = unit.outer;
					}
		
					Some(names.join("."));
				} else {
					None;
				}
			},
			at(type is TypeDecl) => {
				switch getFullPath(type.lookup).map(p -> '$p.${type.name.name}') {
					case p = Some(_): p;
					case None: Some(type.name.name);
				}
			},
			_ => Some("???")
		);
	}

	function simpleName(): String return switch t {
		case TPath(path, _): path.simpleName();
		case TConcrete({lookup: lookup, name: {name: name}, params: []}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name);
		case TConcrete({lookup: lookup, name: {name: name}, params: params}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name)
			+ '[${params.map(_ -> "...").join(", ")}]';
		case TGeneric({name: {name: name}, params: []}): name;
		case TGeneric({name: {name: name}, params: params}): '$name[${params.map(_ -> "...").join(", ")}]';
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
		case TModular(type, _): return type.simpleName();
	}

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil) {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return switch t {
			case TPath(path2, source): throw "NYI!";
			case TConcrete(decl): decl.findType(path, absolute, cache);
			case TThis(span, source): throw "NYI!";
			case TErased(span): None;
			case TMulti(types): throw "NYI!";
			case TApplied(type, params): throw "NYI!";
			case TGeneric(typevar): throw "NYI!";
			case TModular(_, unit): unit.findType(path, absolute, cache);
		}
	}

	function makeTypePath(path: TypePath) {
		return new Type(TPath(path, this));
	}

	@:keep private function __compare(other: Any) {
		return other._match(
			at(type is Type) => (t.equals(type.t) ? 0 : -1),
			_ => -1
		);
	}
}