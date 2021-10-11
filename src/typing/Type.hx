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


function getThisType(idecl: ITypeDecl) return idecl._match(
	at(td is TypeDecl) => td.thisType,
	at(tv is TypeVar) => tv.thisType,
	at(cat is Category) => cat.type.value(),
	_ => throw "bad"
);


function reduceOverloadsBy<T>(overloads: Array<T>, by: (T) -> Type): Array<T> return overloads._match(
	at([]) => [],
	at([t]) => [t],
	_ => {
		final res = [overloads[0]];

		for(i in 1...overloads.length) {
			final ov = overloads[i];
			final type = by(ov);

			if(res.some(ov2 -> {
				final ty = by(ov2);

				switch [ty.t, type.t] {
					case [TApplied(t1, p1), TApplied(t2, p2)]:
						if(t1 != t2) {
							return true;
						} else {
							for(i => p in p1) {
								if(!p.hasChildType(p2[i])) {
									return true;
								}
							}
							return false;
						}
					
					case [TConcrete(d1), TConcrete(d2)]:
						if(d1.name.name != d2.name.name) {
							return !d1.hasParentDecl(d2);
						} else {
							for(i => p in d1.params) {
								if(!p.hasChildType(d2.params[i])) {
									return true;
								}
							}
							return false;
						}

					default:
						if(ty.t != type.t) {
							return !ty.hasParentType(type);
						} else {
							return false;
						}
				}
			})) {
				res.push(ov);
			}
		}

		res;
	}
);

function reduceOverloads(types: Array<Type>): Array<Type> {
	return inline reduceOverloadsBy(types, t -> t);
}


function mostSpecificBy<T>(values: Array<T>, by: (T) -> Type): Array<T> return values._match(
	at([]) => [],
	at([t]) => [t],
	_ => {
		var res = [values[0]];

		for(i in 1...values.length) {
			final v = values[i];
			
			/*function loop(type1: Type, type2: Type): Bool {
				trace(type1.fullName(), type2.fullName());

				if(type1.hasChildType(type2)) return false;

				switch [type1.t, type2.t] {
					case [TThis(t1), _]: return loop(getThisType(t1), type2);
					case [_, TThis(t2)]: return loop(type1, getThisType(t2));

					case [TMulti(types), _]: switch mostSpecific(types) {
						case []: throw "bad";
						case [t1]: return loop(t1, type2);
						case ts: throw "bruh";
					}

					case [_, TMulti(types)]: switch leastSpecific(types) {
						case []: throw "bad";
						case [t2]: return loop(type1, t2);
						case ts: throw "bruh";
					}

					case [TApplied(t1, p1), TApplied(t2, p2)]:
						if(loop(t1, t2)) {
							return false;
						} else {
							return p1.every2(p2, (pa1, pa2) ->
								pa1 == pa2 || !pa1.hasChildType(pa2)
							);
						}
					
					case [TConcrete(d1), TConcrete(d2)]:
						if(d1.name.name != d2.name.name) {
							return !d1.hasChildDecl(d2);
						} else {
							return d1.lookup == d2.lookup && d1.params.every2(d2.params, (p1, p2) ->
								p1 == p2 || p1.hasParentType(p2)
							);
						}

					default:
						if(type1.t != type2.t) {
							return !type1.hasChildType(type2);
						} else {
							return false;
						}
				}
			}
			// current <=> existing
			if(res.some(v2 -> loop(by(v), by(v2)))) {
				res.push(v);
			}*/
			
			final res2 = res.filter(v2 -> {
				final a = by(v);
				final b = by(v2);
				
				!(a.hasParentType(b) && !a.hasChildType(b));
			});
			if(res2.length != res.length) {
				res=res2;
				res.push(v);
			}
		}

		res;
	}
);

function mostSpecific(types: Array<Type>): Array<Type> {
	return inline mostSpecificBy(types, t -> t);
}


function leastSpecificBy<T>(values: Array<T>, by: (T) -> Type): Array<T> return values._match(
	at([]) => [],
	at([t]) => [t],
	_ => {
		var res = [values[0]];

		for(i in 1...values.length) {
			final v = values[i];
			
			final res2 = res.filter(v2 -> {
				final a = by(v);
				final b = by(v2);
				
				!(a.hasChildType(b) && !a.hasParentType(b));
			});
			if(res2.length != res.length) {
				res = res2;
				res.push(v);
			}
		}

		res;
	}
);

function leastSpecific(types: Array<Type>): Array<Type> {
	return inline leastSpecificBy(types, t -> t);
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

	function fullName(): String return switch t {
		case TPath(depth, lookup, _):
			"_.".repeat(depth)
			+ lookup.mapArray((_, n, p) -> n + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName())}]')).join(".");
		case TLookup(type, lookup, _):
			type.fullName()
			+ lookup.mapArray((_, n, p) -> '.$n' + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName())}]')).join("");
		case TConcrete(decl): decl.fullName();
		case TTypeVar(tvar): tvar.fullName();
		case TThis(decl): 'This (${decl.fullName()})';
		case TBlank: "_";
		case TMulti(types): "(" + types.joinMap(" | ", ty -> ty.fullName()) + ")";
		case TApplied(type, params): // Probably bad but eh
			final name = type.fullName();
			(if(name.endsWith("]")) {
				name.removeAfterLast("[");
			} else {
				name;
			}) + "[" + params.map(p -> p.fullName()).join(", ") + "]";
		case TModular(type, _): return type.fullName();
	}


	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return t._match(
			at(TPath(depth, lookup, source)) => throw "NYI!",
			at(TLookup(type, lookup, source)) => throw "NYI!",
			at(TConcrete(decl)) => decl.findType(path, search, from, depth, cache),
			at(TThis(source)) => source.findType(path, search, from, depth, cache),
			at(TBlank) => None,
			at(TMulti(types)) => leastSpecific(types)._match(
				at([]) => throw "bad",
				at([ty]) => ty.findType(path, search, from, depth, cache),
				at(tys) => throw "NYI!"
			),
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(ty -> ty.acceptsArgs(args)))._match(
					at([]) => None,
					at([ty]) => ty.findType(path, search, from, depth, cache),
					at(tys) => throw "NYI!"
				);
			},
			at(TApplied(type, args)) =>
				type.findType(path, search, from, depth, cache),
			at(TTypeVar(typevar)) => throw "NYI!",
			at(TModular(t, unit)) => unit.findType(path, Outside, from, depth, cache)._match(
				at(res = Some(_)) => res,
				at(None) => t.findType(path, search, from, depth, cache)
			)
		);
	}

	/*function findTypeOld(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return switch t {
			case TPath(depth, lookup, source): throw "NYI!";
			case TLookup(type, lookup, source): throw "NYI!";
			case TConcrete(decl): decl.findTypeOld(path, absolute, cache);
			case TThis(source):
				if(absolute) {
					source.findTypeOld(path, absolute, cache);
				} else throw "NYI!";
			case TBlank: None;
			case TMulti(types): throw "NYI!";
			case TApplied(type, params):
				if(absolute) {
					type.findTypeOld(path, absolute, cache);
				} else throw "NYI!";
			case TTypeVar(typevar): throw "NYI!";
			case TModular(t, unit): switch unit.findTypeOld(path, false, cache) {
				case res = Some(_): res;
				case None: t.findTypeOld(path, absolute, cache);
			}
		}
	}*/

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	@:keep private function __compare(other: Any) {
		return other._match(
			at(type is Type) => ((switch [t, type.t] {
				case [TConcrete(decl1), TConcrete(decl2)]: decl1 == decl2;
				case [TThis(src1), TThis(src2)]: src1 == src2;
				case [TModular(t1, u1), TModular(t2, u2)]: u1 == u2 && t1 == t2;
				case [TBlank, TBlank]: true;
				case [_, _]: t.equals(type.t);
			}) ? 0 : -1),
			_ => -1
		);
	}

	function toString() {
		return Std.string(t);
	}


	function isNative(kind: NativeKind) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.isNative(kind),
			at(TThis(decl is TypeDecl)) => decl.isNative(kind),
			at(TThis(_)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.some(ty -> ty.isNative(kind)),
			at(TApplied(type, params)) => type.isNative(kind),
			at(TTypeVar(typevar)) => typevar.isNative(kind),
			at(TModular(type, unit)) => type.isNative(kind)
		);
	}


	function acceptsArgs(args: Array<Type>) {
		return t._match(
			at(TPath(_, _, _)) => throw "bad",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
			at(TThis(source)) => source._match(
				at(decl is TypeDecl) => decl.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
				at(tvar is TypeVar) => tvar.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
				_ => throw "bad"
			),
			at(TBlank) => true,
			at(TMulti(types)) => types.every(type -> type.acceptsArgs(args)),
			at(TApplied(type, args2)) => throw "todo",
			at(TTypeVar(tvar)) => tvar.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
			at(TModular(type, _)) => type.acceptsArgs(args)
		);
	}


	function hasParentDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasParentDecl(decl),
			at(TThis(source)) => source.hasParentDecl(decl),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentDecl(decl)),
			at(TApplied(type, params)) => {
				//trace(type.fullName(), decl.fullName());
				type.hasParentDecl(decl) || decl.params._match(
					at([]) => false,
					at(ps, when(params.length == ps.length)) => {
						if(type.t == decl.thisType.t
						|| type.t._match(
							at(TThis(d1) | TConcrete(d1)) => (d1.lookup == decl.lookup && d1.name.name == decl.name.name),
							_ => false
						)) {
							params.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasParentType(p2));
						} else {
							false;
						}
					},
					_ => false
				);
			},
			at(TTypeVar(typevar)) => typevar.hasParentDecl(decl),
			at(TModular(type, unit)) => type.hasParentDecl(decl)
		);
	}

	function hasChildDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasChildDecl(decl),
			at(TThis(source)) => source.hasChildDecl(decl),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasChildDecl(decl)),
			at(TApplied(type, params)) => {
				type.hasChildDecl(decl) || decl.params._match(
					at([]) => false,
					at(ps, when(params.length == ps.length)) => {
						if(type.t == decl.thisType.t
						|| type.t._match(
							at(TThis(d1) | TConcrete(d1)) => (d1.lookup == decl.lookup && d1.name.name == decl.name.name),
							_ => false
						)) {
							params.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasChildType(p2));
						} else {
							false;
						}
					},
					_ => false
				);
			},
			at(TTypeVar(typevar)) => typevar.hasChildDecl(decl),
			at(TModular(type, unit)) => type.hasChildDecl(decl)
		);
	}


	function hasParentType(type: Type) {
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type2, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasParentType(type),
			at(TThis(source)) => source.hasParentType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentType(type)),
			at(TApplied(type2, params)) => {
				type2.hasParentType(type) || type.t._match(
					at(TApplied(ty, ps), when(params.length == ps.length)) => {
						if(type2.t == ty.t
						|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)) {
							params.every2(ps, (p1, p2) -> p1 == p2 || p1.hasParentType(p2));
						} else {
							false;
						}
					},
					_ => false
				);
			},
			at(TTypeVar(typevar)) => typevar.hasParentType(type),
			at(TModular(type2, unit)) => type2.hasParentType(type)
		);
	}

	function hasChildType(type: Type) {
		//trace(this.fullName(),type.fullName());
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type2, lookup, source)) => throw "todo "+type2.fullName()+" "+lookup,
			at(TConcrete(decl2)) => decl2.hasChildType(type),
			at(TThis(source)) => source.hasChildType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				//trace(types.map(t->t.fullName()));
				reduceOverloads(types).every(ty -> ty.hasChildType(type));
			},
			at(TApplied(type2, params)) => {
				//trace(type2.fullName(), params.map(p->p.fullName()));
				type2.hasChildType(type) || type.t._match(
					at(TApplied(ty, ps), when(params.length == ps.length)) => {
						if(type2.t == ty.t
						|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)) {
							params.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasChildType(p2));
						} else {
							false;
						}
					},
					_ => false
				);
			},
			at(TTypeVar(typevar)) => typevar.hasChildType(type),
			at(TModular(type2, unit)) => type2.hasChildType(type)
		);
	}


	/*function unifyWithType(type: Type): Null<Type> {
		return this.t == type.t ? this : Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => if(d1 == d2) this else null,
			at([TModular(t1, _), _]) => t1.strictUnifyWithType(type),
			at([_, TModular(t2, _)]) => this.strictUnifyWithType(t2),
			_ => null
		);
	}*/
	
	function strictUnifyWithType(type: Type): Null<Type> {
		return this.t == type.t ? this : Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => if(d1 == d2) this else null,
			at([TModular(t1, _), _]) => t1.strictUnifyWithType(type),
			at([_, TModular(t2, _)]) => this.strictUnifyWithType(t2),
			_ => null
		);
	}

	
	function canSeeMember(member: Member) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.canSeeMember(member),
			at(TThis(source)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => throw "todo",
			at(TApplied(type, params)) => type.canSeeMember(member),
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.canSeeMember(member)
		);
	}
	
	function canSeeMethod(method: AnyMethod) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.canSeeMethod(method),
			at(TThis(source)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => throw "todo",
			at(TApplied(type, params)) => type.canSeeMethod(method),
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.canSeeMethod(method)
		);
	}


	function instMembers(from: TypeDecl): Array<Member> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.instMembers(from),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.instMembers(from);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.instMembers(from)).unique();
			},
			at(TApplied(type, params)) => {
				type.instMembers(from);
			},
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.instMembers(from)
		);
	}


	function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findSingleStatic(name, from, getter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findSingleStatic(name, from, getter, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findSingleStatic(name, from, getter, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied(type, params)) => {
				type.findSingleStatic(name, from, getter, cache);
			},
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.findSingleStatic(name, from, getter, cache)
		);
	}


	function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiStaticKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findMultiStatic(names, from, setter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findMultiStatic(names, from, setter, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.findMultiStatic(names, from, setter, cache)).unique();
			},
			at(TApplied(type, params)) => {
				type.findMultiStatic(names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.findMultiStatic(names, from, setter, cache)
		);
	}


	function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleInstKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findSingleInst(name, from, getter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findSingleInst(name, from, getter, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findSingleInst(name, from, getter, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied(type, params)) => {
				type.findSingleInst(name, from, getter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findSingleInst(name, from, getter, cache),
			at(TModular(type, unit)) => type.findSingleInst(name, from, getter, cache)
		);
	}


	function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiInstKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findMultiInst(names, from, setter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findMultiInst(names, from, setter, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.findMultiInst(names, from, setter, cache)).unique();
			},
			at(TApplied(type, params)) => {
				type.findMultiInst(names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findMultiInst(names, from, setter, cache),
			at(TModular(type, unit)) => type.findMultiInst(names, from, setter, cache)
		);
	}

	
	function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil): Array<CastMethod> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findCast(target, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findCast(target, from, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => leastSpecific(types).flatMap(t -> t.findCast(target, from, cache)),
			at(TApplied(type, params)) => {
				type.findCast(target, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findCast(target, from, cache),
			at(TModular(type, unit)) => type.findCast(target, from, cache)
		);
	}


	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findCategory(cat, forType, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findCategory(cat, forType, from, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).flatMap(t -> t.findCategory(cat, forType, from, cache)),
			at(TApplied(type, params)) => {
				type.findCategory(cat, forType, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findCategory(cat, forType, from, cache),
			at(TModular(type, unit)) => unit.findCategory(cat, forType, from, cache.prepend(type)).concat(type.findCategory(cat, forType, from, cache.prepend(unit)))
		);
	}


	function findThisCategory(cat: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findThisCategory(cat, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findThisCategory(cat, from, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).flatMap(t -> t.findThisCategory(cat, from, cache)),
			at(TApplied(type, params)) => {
				type.findThisCategory(cat, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findThisCategory(cat, from, cache),
			at(TModular(type, unit)) => unit.findCategory(cat, type, from, cache.prepend(type)).concat(type.findThisCategory(cat, from, cache.prepend(unit)))
		);
	}


	function getMostSpecific(): Type {
		return t._match(
			at(TMulti(types)) => mostSpecific(types)._match(
				at([]) => throw "bad",
				at([type]) => type,
				at(types2) => if(types2.length == types.length) this else {t: TMulti(types2), span: span}
			),
			at(TApplied({t: TMulti(types)}, params)) => mostSpecific(types.map(ty -> ty.getMostSpecific()))._match(
				at([]) => throw "bad",
				at([ty]) => {t: TApplied(ty, params), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, params), span: span}
			),
			//types.filter(type -> type.hasChildType)
			_ => this
		);
	}

	function getLeastSpecific(): Type {
		return t._match(
			at(TMulti(types)) => leastSpecific(types)._match(
				at([]) => throw "bad",
				at([type]) => type,
				at(types2) => if(types2.length == types.length) this else {t: TMulti(types2), span: span}
			),
			at(TApplied({t: TMulti(types)}, params)) => leastSpecific(types.map(ty -> ty.getLeastSpecific()))._match(
				at([]) => throw "bad",
				at([ty]) => {t: TApplied(ty, params), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, params), span: span}
			),
			//types.filter(type -> type.hasChildType)
			_ => this
		);
	}
}