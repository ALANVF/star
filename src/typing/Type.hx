package typing;

import text.Span;

using hx.strings.Strings;

enum TypeKind {
	TPath(path: TypePath, source: ILookupType);
	TConcrete(decl: TypeDecl);
	TThis(span: Option<Span>, source: ITypeDecl);
	TErased(span: Span);
	TMulti(types: Array<Type>);
	TApplied(type: Type, params: Array<Type>);
	TGeneric(generic: Generic);
	TModular(type: Type, unit: Unit);
}

@:build(util.Auto.build())
class Type {
	var t: TypeKind;

	function new(t: TypeKind) {
		this.t = t;
	}
	
	static function getFullPath(lookup: ILookupType): Option<String> {
		return if(lookup is File) {
			var file = cast(lookup, File);
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
		} else if(lookup is TypeDecl) {
			final type = cast(lookup, TypeDecl);
			switch getFullPath(type.lookup).map(p -> '$p.${type.name.name}') {
				case p = Some(_): p;
				case None: Some(type.name.name);
			}
		} else {
			Some("???");
		}
	}

	function simpleName(): String return switch t {
		case TPath(path, _): path.simpleName();
		case TConcrete({lookup: lookup, name: {name: name}, params: None}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name);
		case TGeneric({name: {name: name}, params: None}): name;
		case TConcrete({lookup: lookup, name: {name: name}, params: Some(params)}):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name)
			+ '[${params.map(_ -> "...").join(", ")}]';
		case TGeneric({name: {name: name}, params: Some(params)}): '$name[${params.map(_ -> "...").join(", ")}]';
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

	function findType(path: List<String>, absolute = false, cache: List<{}> = Nil) {
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
			case TGeneric(generic): throw "NYI!";
			case TModular(_, unit): unit.findType(path, absolute, cache);
		}
	}

	function makeTypePath(path: TypePath) {
		return new Type(TPath(path, this));
	}

	@:keep private function __compare(other: Dynamic) {
		return switch Std.downcast(other, Type) {
			case null: -1;
			case type: t.equals(type.t) ? 0 : -1;
		}
	}
}