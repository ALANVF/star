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
					case [TApplied({t: TConcrete(d1)}, p1), TApplied({t: TConcrete(d2)}, p2)] if(p1.length == p2.length):
						if(d1.name.name == d2.name.name && d1.lookup == d2.lookup && d1.declName() == d2.declName()) {
							for(i => p in p1) {
								if(!p.hasChildType(p2[i])) {
									return true;
								}
							}
							return false;
						} else {
							return true;
						}

					case [TApplied(t1, p1), TApplied(t2, p2)] if(p1.length == p2.length):
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
			
			final res2 = res.filter(v2 -> {
				final parent = by(v);
				final child = by(v2);
				
				parent.hasParentType(child) && !parent.hasChildType(child);
			});
			if(res2.length != res.length) {
				res = res2;
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
				final parent = by(v);
				final child = by(v2);
				
				parent.hasChildType(child) || !parent.hasParentType(child);
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
				name.removeAfter("[");
			} else {
				name;
			}) + "[" + params.map(p -> p.simpleName()).join(", ") + "]";
		case TModular(type, _): return type.simpleName();
	}

	function fullName(cache: List<Type> = Nil): String {
		return if(cache.contains(this)) {
			this.simpleName();
		} else {
			cache = cache.prepend(this);
			switch t {
				case TPath(depth, lookup, _):
					"_.".repeat(depth)
					+ lookup.mapArray((_, n, p) -> n + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName(cache))}]')).join(".");
				case TLookup(type, lookup, _):
					type.fullName()
					+ lookup.mapArray((_, n, p) -> '.$n' + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName(cache))}]')).join("");
				case TConcrete(decl): decl.fullName(cache);
				case TTypeVar(tvar): tvar.fullName(cache);
				case TThis(decl): 'This (${decl.fullName(cache)})';
				case TBlank: "_";
				case TMulti(types): "(" + types.joinMap(" | ", ty -> ty.fullName(cache)) + ")";
				case TApplied(type, params): // Probably bad but eh
					final name = type.fullName(cache);
					//if(type.t.match(TMulti(_))) trace(name);
					(if(name.endsWith("]")) {
						name.removeAfter("[");
					} else {
						name;
					}) + ":[" + params.map(p -> p.fullName(cache)).join(", ") + "]";
				case TModular(type, _): return type.fullName(cache);
			}
		}
	}


	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return t._match(
			at(TPath(_, l, _)) => {
				//trace(l.span().display(), l.simpleName(), path.simpleName());
				this.t=this.simplify().t;
				this.findType(path, search, from, depth, cache);
			},
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
				at(None) => t.findType(path, search, from, depth, cache.prepend(unit))
			)
		);
	}

	function makeTypePath(path: TypePath):Type {
		//return if(t.match(TPath(_, _, _))) throw "bad" else path.toType(this.simplify());
		throw "bad";
	}

	@:keep private function __compare(other: Any) {
		return other._match(
			at(type is Type) => ((switch [t, type.t] {
				case [TConcrete(decl1), TConcrete(decl2)]: decl1 == decl2;
				case [TThis(src1), TThis(src2)]: src1 == src2;
				case [TModular(t1, u1), TModular(t2, u2)]: u1 == u2 && t1 == t2;
				case [TModular(t1, _), _]: t1 == type;
				case [_, TModular(t2, _)]: this == t2;
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
			at(TConcrete(decl)) => {
				//trace(decl.name.name, decl.params.map(t->t.fullName()), args.map(t->t.fullName()));
				decl.params.every2Strict(args, (p, a) -> {
					p == a ? true : p.t._match(
						at(TApplied({t: TMulti(types)}, ps)) => {
							if(types.contains(this)) {
								true;
							} else {
								p.hasChildType(a);
							}
						},
						_ => p.hasChildType(a)
					);
				});
			},
			at(TThis(source)) => source._match(
				at(decl is TypeDecl) => decl.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
				at(tvar is TypeVar) => tvar.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
				_ => throw "bad"
			),
			at(TBlank) => true,
			at(TMulti(types)) => {
				mostSpecific(types).every(type -> type.acceptsArgs(args));
			},
			at(TApplied(type, args2)) => throw "todo",
			at(TTypeVar(tvar)) => tvar.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
			at(TModular(type, _)) => type.acceptsArgs(args)
		);
	}


	function hasParentDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasParentDecl(decl),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasParentDecl(decl),
			at(TThis(source)) => source.hasParentDecl(decl),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentDecl(decl)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasParentDecl(decl)),
			at(TApplied(type, args)) => {
				type.acceptsArgs(args) && (type.hasParentDecl(decl) || decl.params._match(
					at([]) => false,
					at(ps, when(args.length == ps.length)) => {
						if(type.t == decl.thisType.t
						|| type.t._match(
							at(TThis(d1) | TConcrete(d1)) => (d1.lookup == decl.lookup && d1.name.name == decl.name.name),
							_ => false
						)) {
							args.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasParentType(p2));
						} else {
							false;
						}
					},
					_ => false
				));
			},
			at(TTypeVar(typevar)) => typevar.hasParentDecl(decl),
			at(TModular(type, unit)) => type.hasParentDecl(decl)
		);
	}

	function hasChildDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasChildDecl(decl),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasChildDecl(decl),
			at(TThis(source)) => source.hasChildDecl(decl),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasChildDecl(decl)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasChildDecl(decl)),
			at(TApplied(type, args)) => {
				type.acceptsArgs(args) && (type.hasChildDecl(decl) || decl.params._match(
					at([]) => false,
					at(ps, when(args.length == ps.length)) => {
						if(type.t == decl.thisType.t
						|| type.t._match(
							at(TThis(d1) | TConcrete(d1)) => (d1.lookup == decl.lookup && d1.name.name == decl.name.name),
							_ => false
						)) {
							args.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasChildType(p2));
						} else {
							false;
						}
					},
					_ => false
				));
			},
			at(TTypeVar(typevar)) => typevar.hasChildDecl(decl),
			at(TModular(type, unit)) => type.hasChildDecl(decl)
		);
	}


	function hasParentType(type: Type) {
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasParentType(type),
			at(TLookup(type2, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasParentType(type),
			at(TThis(source)) => source.hasParentType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentType(type)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasParentType(type)),
			at(TApplied(type2, args)) => {
				type2.acceptsArgs(args) && (type2.hasParentType(type) || type.t._match(
					at(TApplied(ty, ps), when(args.length == ps.length && ty.acceptsArgs(ps))) => {
						if(type2.t == ty.t
						|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)
						|| type2.hasParentType(ty)) {
							args.every2(ps, (p1, p2) -> p1 == p2 || p1.hasParentType(p2));
						} else {
							false;
						}
					},
					at(TApplied(_, _)) => false,
					_ => type2.acceptsArgs(args)
					//_ => false
				));
			},
			at(TTypeVar(typevar)) => typevar.hasParentType(type),
			at(TModular(type2, unit)) => type2.hasParentType(type)
		);
	}

	function hasChildType(type: Type) {
		//trace(this.fullName(),type.fullName());
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasChildType(type),
			at(TLookup(type2, lookup, source)) => throw "todo "+type2.fullName()+" "+lookup,
			at(TConcrete(decl2)) => decl2.hasChildType(type),
			at(TThis(source)) => source.hasChildType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasChildType(type)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasChildType(type)),
			at(TApplied(type2, args)) => {
				//trace(type2.fullName(), args.map(p->p.fullName()));
				type2.acceptsArgs(args) && (type2.hasChildType(type) || type.t._match(
					at(TApplied(ty, ps), when(args.length == ps.length && ty.acceptsArgs(ps))) => {
						if(type2.t == ty.t
						/*|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)*/ || type2.hasChildType(ty)) {
							args.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasChildType(p2));
						} else {
							false;
						}
					},
					at(TApplied(_, _)) => false,
					_ => {
						//trace(type.fullName(),args.map(a->a.fullName()));
						type.acceptsArgs(args);
					}
					//_ => false
				));
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
			at([TApplied(t1, a1), TApplied(t2, a2)], when(a1.length == a2.length)) => {
				t1.strictUnifyWithType(t2)._and(t => {
					{t: TApplied(t, [for(i => a1_ in a1) {
						a1_.strictUnifyWithType(a2[i])._match(
							at(a!) => a,
							_ => return null
						);
					}])};
				});
			},
			at([TMulti(types1), TMulti(types2)]) => {
				if(types1.length == types2.length && types1.sorted((a, b) -> a.__compare(b)).every2(types2.sorted((a, b) -> a.__compare(b)), (t1, t2) -> t1.strictUnifyWithType(t2) != null)) {
					this;
				} else {
					null;
				}
			},
			at([TTypeVar(tv1), TTypeVar(tv2)]) => {
				if(tv1.strictUnifyWithTypevar(tv2)) {
					this;
				} else {
					null;
				}
			},
			at([TThis(t1), TThis(t2)]) => {
				if(t1 == t2) {
					this;
				} else {
					null;
				}
			},
			at([TThis(t1), TConcrete(d2)]) => {
				if(t1 == d2) {
					type;
				} else {
					null;
				}
			},
			at([TConcrete(d1), TThis(t2)]) => {
				if(d1 == t2) {
					this;
				} else {
					null;
				}
			},
			_ => null
		);
	}

	function hasStrictChildType(type: Type) {
		return this.t == type.t || Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => d1.hasChildDecl(d2),
			at([TModular(t1, _), _]) => t1.hasStrictChildType(type),
			at([_, TModular(t2, _)]) => this.hasStrictChildType(t2),
			_ => false
		);
	}

	
	function canSeeMember(member: Member) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.canSeeMember(member),
			at(TThis(source)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.every(ty -> ty.canSeeMember(member)),
			at(TApplied(type, args)) => type.canSeeMember(member),
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
			at(TMulti(types)) => types.every(ty -> ty.canSeeMethod(method)),
			at(TApplied(type, args)) => type.canSeeMethod(method),
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.canSeeMethod(method)
		);
	}


	function instMembers(from: ITypeDecl): Array<Member> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.instMembers(from),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.instMembers(from);
					} else {
						//throw "???";
						td.instMembers(from);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.instMembers(from)).unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.instMembers(from))
					.unique();
			},
			at(TApplied(type, args)) => {
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
						//throw "???";
						td.findSingleStatic(name, from, getter, cache);
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
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findSingleStatic(name, from, getter, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
				type.findSingleStatic(name, from, getter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findSingleStatic(name, from, getter, cache),
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
						//throw "???";
						td.findMultiStatic(names, from, setter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad "+span._and(s=>s.display()),
			at(TMulti(types)) => {
				leastSpecific(types)
					.flatMap(type -> type.findMultiStatic(names, from, setter, cache))
					.unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findMultiStatic(names, from, setter, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findMultiStatic(names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findMultiStatic(names, from, setter, cache),
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
						//throw "???";
						td.findSingleInst(name, from, getter, cache);
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
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findSingleInst(name, from, getter, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
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
						//throw "???"+names+from.fullName()+" "+td.fullName();
						td.findMultiInst(names, from, setter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				types
					.flatMap(type -> type.findMultiInst(names, from, setter, cache))
					.unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				types
					.filter(type -> type.acceptsArgs(args))
					.flatMap(type -> type.findMultiInst(names, from, setter, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findMultiInst(names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findMultiInst(names, from, setter, cache),
			at(TModular(type, unit)) => type.findMultiInst(names, from, setter, cache)
		);
	}

	
	function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil): Array<CastKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findCast(target, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td.thisType == from.thisType) {
						td.findCast(target, from, cache);
					} else {
						//throw "???"+td.fullName()+" "+from.fullName()+" "+target.fullName();
						td.findCast(target, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => leastSpecific(types).flatMap(t -> t.findCast(target, from, cache)),
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findCast(target, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findCast(target, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findCast(target, from, cache),
			at(TModular(type, unit)) => type.findCast(target, from, cache)
		);
	}


	function findUnaryOp(op: UnaryOp, from: ITypeDecl, cache: List<Type> = Nil): Null<UnaryOpKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findUnaryOp(op, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findUnaryOp(op, from, cache);
					} else {
						//throw "???";
						td.findUnaryOp(op, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findUnaryOp(op, from, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findUnaryOp(op, from, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
				type.findUnaryOp(op, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findUnaryOp(op, from, cache),
			at(TModular(type, unit)) => type.findUnaryOp(op, from, cache)
		);
	}


	function findBinaryOp(op: BinaryOp, from: ITypeDecl, cache: List<Type> = Nil): Array<BinaryOpKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findBinaryOp(op, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findBinaryOp(op, from, cache);
					} else {
						//throw "???";
						td.findBinaryOp(op, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.findBinaryOp(op, from, cache)).unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findBinaryOp(op, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findBinaryOp(op, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findBinaryOp(op, from, cache),
			at(TModular(type, unit)) => type.findBinaryOp(op, from, cache)
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
			at(TApplied({t: TMulti(types)}, args)) => {
				reduceOverloads(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findCategory(cat, forType, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
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
			at(TApplied({t: TMulti(types)}, args)) => mostSpecific(types.filter(ty -> ty.acceptsArgs(args)).map(ty -> ty.getMostSpecific()))._match(
				at([]) => throw "bad",
				at([ty]) => {t: TApplied(ty, args), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, args), span: span}
			),
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
			at(TApplied({t: TMulti(types)}, args)) => {
				final args2 = args.map(a -> a.t._match(
					at(TApplied({t: TMulti(types2)}, as)) => {
						if(types.every2Strict(types2, (t1, t2) -> t1 == t2)) {
							trace(types);
							this;
						} else {
							a;
						}
					},
					_ => a
				));
				leastSpecific(types.filter(ty -> ty.acceptsArgs(args2)).map(ty -> ty.getLeastSpecific()))._match(
					at([]) => throw "bad",
					at([ty]) => {t: TApplied(ty, args), span: span},
					at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, args), span: span}
				);
			},
			_ => this
		);
	}

	function simplify(): Type {
		return t._match(
			at(TPath(depth, lookup, source)) => {
				source.findType(lookup, Start, source._match(
					at(m is AnyMethod) => m.decl,
					at(m is EmptyMethod) => m.decl,
					_ => cast source
				), depth)._match(
					at(Some(ty)) => {this.t=ty.t;ty;},
					at(None) => throw 'error: type `${this.fullName()}` does not exist! ${lookup.span().display()}'
				);
			},
			at(TApplied({t: TMulti(types)}, args)) => mostSpecific(types.filter(ty -> ty.acceptsArgs(args)))._match(
				at([]) => throw "bad",
				at([ty]) => {t: TApplied(ty, args), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, args), span: span}
			),
			at(TApplied(ty, args)) => {t: TApplied(ty.simplify(), args.map(a -> a.simplify())), span: span},
			_ => this
		);
	}
}