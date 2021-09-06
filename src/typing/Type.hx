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
	TPath(depth: Int, lookup: LookupPath, source: ILookupType);
	TLookup(type: Type, lookup: LookupPath, source: ILookupType);
	TConcrete(decl: TypeDecl);
	TThis(source: ITypeDecl);
	TBlank;
	TMulti(types: Array<Type>);
	TApplied(type: Type, params: Array<Type>);
	TTypeVar(typevar: TypeVar);
	TModular(type: Type, unit: Unit);
}

@:structInit
@:build(util.Auto.build())
class Type {
	var t: TypeKind;
	var span: Null<Span> = null;

	function new(t: TypeKind, ?span: Span) {
		this.t = t;
		this.span = span;
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
		case TPath(depth, lookup, _):
			"_.".repeat(depth)
			+ lookup.mapArray((_, n, p) -> n + (p.length == 0 ? "" : '[${p.joinMap(", ", _ -> "...")}]')).join(".");
		case TLookup(type, lookup, _):
			type.simpleName()
			+ lookup.mapArray((_, n, p) -> '.$n' + (p.length == 0 ? "" : '[${p.joinMap(", ", _ -> "...")}]')).join("");
		case TConcrete({lookup: lookup, name: {name: name}, params: []}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name);
		case TConcrete({lookup: lookup, name: {name: name}, params: params}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name)
			+ '[${params.map(_ -> "...").join(", ")}]';
		case TTypeVar({name: {name: name}, params: []}): name;
		case TTypeVar({name: {name: name}, params: params}): '$name[${params.map(_ -> "...").join(", ")}]';
		case TThis(_): "This";
		case TBlank: "_";
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

	function findType(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return switch t {
			case TPath(depth, lookup, source): throw "NYI!";
			case TLookup(type, lookup, source): throw "NYI!";
			case TConcrete(decl): decl.findType(path, absolute, cache);
			case TThis(_): throw "NYI!";
			case TBlank: None;
			case TMulti(types): throw "NYI!";
			case TApplied(type, params): throw "NYI!";
			case TTypeVar(typevar): throw "NYI!";
			case TModular(t, unit): switch unit.findType(path, false, cache) {
				case res = Some(_): res;
				case None: t.findType(path, absolute, cache);
			}
		}
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	@:keep private function __compare(other: Any) {
		return other._match(
			at(type is Type) => (t.equals(type.t) ? 0 : -1),
			_ => -1
		);
	}

	function toString() {
		return Std.string(t);
	}
}