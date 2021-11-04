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
	TPath(depth: Int, lookup: LookupPath, source: ITypeLookup);
	TLookup(type: Type, lookup: LookupPath, source: ITypeLookup);
	TConcrete(decl: TypeDecl);
	TInstance(decl: TypeDecl, params: Array<Type>, ctx: TypeVarCtx);
	TThis(source: AnyTypeDecl);
	TBlank;
	TMulti(types: Array<Type>);
	TApplied(type: Type, params: Array<Type>);
	TTypeVar(typevar: TypeVar);
	TModular(type: Type, unit: Unit);
}


/*function getThisType(idecl: AnyTypeDecl) return idecl._match(
	at(td is TypeDecl) => td.thisType,
	at(tv is TypeVar) => tv.thisType,
	at(cat is Category) => cat.type.value(),
	_ => throw "bad"
);*/


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
					
					// TODO: TInstance

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
class Type implements ITypeable {
	var t: TypeKind;
	var span: Null<Span> = null;

	function new(t: TypeKind, ?span: Span) {
		this.t = t;
		this.span = span;
	}
	
	static function getFullPath(lookup: ITypeLookup): Option<String> {
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
		
		case TInstance({lookup: lookup, name: {name: name}}, params, _):
			getFullPath(lookup).map(p -> '$p.$name').orElse(name)
			+ '[${params.map(_ -> '...').join(", ")}]';
		
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

	function fullName(cache: TypeCache = Nil): String {
		return if(cache.contains(this)) {
			this.simpleName();
		} else {
			cache += this;
			switch t {
				case TPath(depth, lookup, _):
					"\\"
					+ "_.".repeat(depth)
					+ lookup.mapArray((_, n, p) -> n + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName(cache))}]')).join(".");
				
				case TLookup(type, lookup, _):
					type.fullName()
					+ "\\"
					+ lookup.mapArray((_, n, p) -> '.$n' + (p.length == 0 ? "" : '[${p.joinMap(", ", p -> p.fullName(cache))}]')).join("");
				
				case TConcrete(decl): decl.fullName(cache);

				case TInstance(decl, params, _):
					cache += decl.thisType;
					getFullPath(decl).value() + '.[${params.joinMap(", ", p -> p.fullName(cache))}]';
				
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


	function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		if(cache.contains(this)) {
			return null;
		} else {
			cache += this;
		}

		return t._match(
			at(TPath(_, l, _)) => {
				//trace(l.span().display(), l.simpleName(), path.simpleName());
				this.t=this.simplify().t;
				this.findType(path, search, from, depth, cache);
			},
			at(TLookup(type, lookup, source)) => throw "NYI!",
			at(TConcrete(decl)) => decl.findType(path, search, from, depth, cache),
			at(TInstance(decl, _, tctx)) => decl.findType(path, search, from, depth, cache)._and(ty => ty.getIn(tctx)),
			at(TThis(source)) => source.findType(path, search, from, depth, cache),
			at(TBlank) => null,
			at(TMulti(types)) => leastSpecific(types)._match(
				at([]) => throw "bad",
				at([ty]) => ty.findType(path, search, from, depth, cache),
				at(tys) => throw "NYI!"
			),
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(ty -> ty.acceptsArgs(args)))._match(
					at([]) => null,
					at([ty]) => ty.findType(path, search, from, depth, cache),
					at(tys) => throw "NYI!"
				);
			},
			at(TApplied(type, args)) =>
				type.findType(path, search, from, depth, cache),
			at(TTypeVar(typevar)) => throw "NYI!",
			at(TModular(t, unit)) => unit.findType(path, Outside, from, depth, cache)._match(
				at(res!) => res,
				_ => t.findType(path, search, from, depth, cache + unit)
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
				// TODO: TInstance
				case [TInstance(decl1, params1, _), TInstance(decl2, params2, _)]:
					decl1 == decl2
					&& params1.length == params2.length
					&& params1.every2(params2, (p1, p2) -> p1 == p2);
				case [TThis(src1), TThis(src2)]: src1 == src2;
				case [TTypeVar(tv1), TTypeVar(tv2)]: tv1 == tv2;
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


	// Type resolution

	function maybeIn(ctx: TypeVarCtx): Null<Type> {
		t._match(
			at(TPath(_, _, _) | TLookup(_, _, _)) => {
				return this.simplify().maybeIn(ctx);
			},
			
			at(TConcrete(_) | TInstance(_, _, _) | TThis(_) | TBlank) => {
				return this;
			},

			at(TMulti(types)) => {
				final types2 = [];

				for(type in types) {
					type.maybeIn(ctx)._and(ty => {
						types2.push(ty);
					});
				}

				return types2._match(
					at([]) => null,
					at([type]) => type,
					_ => {t: TMulti(types2), span: span}
				);
			},

			at(TApplied({t: TMulti(types)}, args)) => {
				final types2 = [];
				final args2 = args.map(a -> a.getIn(ctx));

				for(type in types) {
					type.maybeIn(ctx)._and(type2 => {
						type2.applyArgs(args2)._and(type3 => {
							types2.push(type3);
						});
					});
				}

				return types2._match(
					at([]) => null,
					at([type]) => type,
					_ => {t: TMulti(types2), span: span}
				);
			},
			
			at(TApplied(type, args)) => {
				return type.maybeIn(ctx)._and(type2 => {
					type2.applyArgs(args.map(a -> a.getIn(ctx)));
				});
			},

			at(TTypeVar(typevar)) => {
				return ctx[typevar]._or(
					this
				);
			},

			at(TModular(type, _)) => {
				return type.maybeIn(ctx);
			}
		);
	}
	
	function getIn(ctx: TypeVarCtx): Type {
		return this.maybeIn(ctx)._andOr(
			ty => ty,
			throw 'Error: invalid type `${this.fullName()}`!'
		);
	}


	function maybeFrom(type: Type): Null<Type> {
		t._match(
			at(TPath(_, _, _) | TLookup(_, _, _)) => {
				return this.simplify().maybeFrom(type);
			},
			
			at(TConcrete(_) | TBlank) => {
				return this;
			},

			at(TInstance(decl, params, ctx)) => {
				return {
					t: TInstance(
						decl,
						params.map(p -> p.getFrom(type)),
						[for(k => v in ctx) k => v.getIn(ctx)]
					),
					span: span
				};
			},

			at(TThis(decl)) => {
				// TODO: check if type <: decl?
				return type;
			},

			at(TMulti(types)) => {
				final types2 = [];

				for(type0 in types) {
					type0.maybeFrom(type)._and(ty => {
						types2.push(ty);
					});
				}

				return types2._match(
					at([]) => null,
					at([type0]) => type0,
					_ => {t: TMulti(types2), span: span}
				);
			},

			at(TApplied({t: TMulti(types)}, args)) => {
				final types2 = [];
				final args2 = args.map(a -> a.maybeFrom(type));

				for(type0 in types) {
					type0.maybeFrom(type)._and(type2 => {
						type2.applyArgs(args2)._and(type3 => {
							types2.push(type3);
						});
					});
				}

				return types2._match(
					at([]) => null,
					at([type0]) => type0,
					_ => {t: TMulti(types2), span: span}
				);
			},
			
			at(TApplied(type0, args)) => {
				return type0.maybeFrom(type)._and(type2 => {
					type2.applyArgs(args.map(a -> a.getFrom(type)));
				});
			},

			at(TTypeVar(typevar)) => return type.t._match(
				at(TInstance(decl, params, ctx)) => {
					ctx[typevar]._or(
						this
					);
				},
				_ => {
					trace(this.fullName(), type.fullName(), span._and(s=>s.display()));
					this;
				}
			),

			at(TModular(type0, _)) => {
				return type0.maybeFrom(type);
			}
		);
	}
	
	function getFrom(type: Type): Type {
		return this.maybeFrom(type)._andOr(
			ty => ty,
			throw 'Error: invalid type `${this.fullName()}`!'
		);
	}
	
	
	// Type checking
	
	function hasParentDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasParentDecl(decl),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasParentDecl(decl),
			at(TInstance(decl2, params, _)) => decl2.hasParentDecl(decl), // TODO
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
			at(TInstance(decl2, params, _)) => decl2.hasChildDecl(decl), // TODO
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
			at(TInstance(decl2, params, _)) => decl2.hasParentType(type), // TODO
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
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasChildType(type),
			at(TLookup(type2, lookup, source)) => throw "todo "+type2.fullName()+" "+lookup,
			at(TConcrete(decl2)) => decl2.hasChildType(type),
			at(TInstance(decl2, params, _)) => decl2.hasChildType(type), // TODO
			at(TThis(source)) => source.hasChildType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasChildType(type)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasChildType(type)),
			at(TApplied(type2, args)) => {
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
					_ => type.acceptsArgs(args)
				));
			},
			at(TTypeVar(typevar)) => typevar.hasChildType(type),
			at(TModular(type2, unit)) => type2.hasChildType(type)
		);
	}


	function hasStrictChildType(type: Type) {
		return this.t == type.t || Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => d1.hasChildDecl(d2),
			// TODO: TInstance
			at([TModular(t1, _), _]) => t1.hasStrictChildType(type),
			at([_, TModular(t2, _)]) => this.hasStrictChildType(t2),
			_ => false
		);
	}


	function hasRefinementDecl(decl: TypeDecl) {
		return t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasRefinementDecl(decl),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl2)) => decl2.hasRefinementDecl(decl),
			at(TInstance(decl2, params, _)) => decl2.hasRefinementDecl(decl), // TODO
			at(TThis(source)) => source.hasRefinementDecl(decl),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasRefinementDecl(decl)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasRefinementDecl(decl)),
			at(TApplied(type, args)) => {
				type.acceptsArgs(args) && (type.hasRefinementDecl(decl) || decl.params._match(
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
			at(TTypeVar(typevar)) => typevar.hasRefinementDecl(decl),
			at(TModular(type, unit)) => type.hasRefinementDecl(decl)
		);
	}

	function hasRefinementType(type: Type) {
		return this.t == type.t || t._match(
			at(TPath(depth, lookup, source)) => this.simplify().hasRefinementType(type),
			at(TLookup(type2, lookup, source)) => throw "todo "+type2.fullName()+" "+lookup,
			at(TConcrete(decl2)) => decl2.hasRefinementType(type),
			at(TInstance(decl2, params, _)) => decl2.hasRefinementType(type), // TODO
			at(TThis(source)) => source.hasRefinementType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasRefinementType(type)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).some(ty -> ty.hasRefinementType(type)),
			at(TApplied(type2, args)) => {
				type2.acceptsArgs(args) && (type2.hasRefinementType(type) || type.t._match(
					at(TApplied(ty, ps), when(args.length == ps.length && ty.acceptsArgs(ps))) => {
						if(type2.t == ty.t
						/*|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)*/ || type2.hasRefinementType(ty)) {
							args.every2(ps, (p1, p2) -> p1.t == p2.t || p1.hasChildType(p2));
						} else {
							false;
						}
					},
					at(TApplied(_, _)) => false,
					_ => type.acceptsArgs(args)
				));
			},
			at(TTypeVar(typevar)) => typevar.hasRefinementType(type),
			at(TModular(type2, unit)) => type2.hasRefinementType(type)
		);
	}

	
	// Unification

	function strictUnifyWithType(type: Type): Null<Type> {
		return this.t == type.t ? this : Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => if(d1 == d2) this else null,
			at([TInstance(decl1, params1, _), TInstance(decl2, params2, _)]) => {
				if(decl1 == decl2
				&& params1.length == params2.length
				&& params1.every2(params2, (p1, p2) -> p1.strictUnifyWithType(p2) != null)) this else null;
			},
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


	// Generics

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

			at(TInstance(decl2, params, _)) => false,//throw "todo "+this.fullName()+" "+span._and(s=>s.display()),
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

	function applyArgs(args: Array<Type>): Null<Type> {
		return t._match(
			at(TPath(_, _, _)) => throw "bad",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.applyArgs(args),
			at(TInstance(_, _, _)) => throw "todo", // partially-applied type
			at(TThis(_) | TBlank) => {t: TApplied(this, args)},
			at(TMulti(types)) => {
				mostSpecific(types).filterMap(ty -> ty.applyArgs(args))._match(
					at([]) => null,
					at([ty]) => ty,
					at(tys) => {t: TMulti(tys)}
				);
			},
			at(TApplied({t: TThis(decl)}, args2)) => {
				trace("todo "+span._and(s=>s.display()));
				decl.applyArgs(args);
			},
			at(TApplied(type, params)) => throw "todo"+this.fullName()+" "+args, // partially-applied type
			at(TTypeVar(typevar)) => throw "todo", // higher-kinded type
			at(TModular(type, _)) => type.applyArgs(args)
		);
	}


	// Binding

	function bindTo(onto: Type, ctx: TypeVarCtx): Null<Type> {
		return Util._match([this.t, onto.t],
			at([TPath(_, _, _), _] | [TLookup(_, _, _), _]
			| [_, TPath(_, _, _)] | [_, TLookup(_, _, _)]) => throw "todo",

			at([_, TTypeVar(typevar)]) => {
				ctx[typevar]._match(
					at(type!) => this.strictUnifyWithType(type),
					_ => {
						ctx[typevar] = this;
						this;
					}
				);
			},

			// this unfortunately looses some type info. can't really do anything about it rn
			at([TConcrete({params: [], type: type} is DirectAlias), _]) => type.simplify().bindTo(onto, ctx),
			at([_, TConcrete({params: [], type: type} is DirectAlias)]) => this.bindTo(type.simplify(), ctx),
			
			at([TConcrete(decl1), TConcrete(decl2)]) => {
				if(decl2.hasChildDecl(decl1)) {
					this;
				} else {
					null;
				}
			},

			// TODO: account for generic typebars and HKTs
			at([_, TConcrete(decl)]) => {
				if(decl.hasChildType(this)) {
					this;
				} else {
					null;
				}
			},

			at([TApplied(base1, args1), TApplied(base2, args2)]) => if(args1.length == args2.length) {
				base1.bindTo(base2, ctx)._match(
					at(base!) => {
						final args = [];

						for(i in 0...args1.length) {
							args1[i].bindTo(args2[i], ctx)._match(
								at(arg!) => args.push(arg),
								_ => return null
							);
						}

						{t: TApplied(base, args), span: span}
					},
					_ => null
				);
			} else null,

			// TODO: somehow delay this from happening because we need type refinement
			at([TTypeVar(typevar), _]) => if(typevar.hasParentType(onto)) this else null,

			at([_, _]) => throw "todo "+this.fullName()+" "+onto.fullName()+" "+span._and(s=>s.display())
		);
	}


	// Attributes

	function isNative(kind: NativeKind) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.isNative(kind),
			at(TThis(decl is TypeDecl)) => decl.isNative(kind),
			at(TThis(_)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.some(ty -> ty.isNative(kind)),
			at(TApplied(type, params)) => type.isNative(kind),
			at(TTypeVar(typevar)) => typevar.isNative(kind),
			at(TModular(type, unit)) => type.isNative(kind)
		);
	}

	function isFlags() {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.isFlags(),
			at(TThis(decl is TypeDecl)) => decl.isFlags(),
			at(TThis(_)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.some(ty -> ty.isFlags()),
			at(TApplied(type, params)) => type.isFlags(),
			at(TTypeVar(typevar)) => typevar.isFlags(),
			at(TModular(type, unit)) => type.isFlags()
		);
	}

	function isStrong() {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.isStrong(),
			at(TThis(decl is TypeDecl)) => decl.isStrong(),
			at(TThis(_)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.some(ty -> ty.isStrong()),
			at(TApplied(type, params)) => type.isStrong(),
			at(TTypeVar(typevar)) => typevar.isStrong(),
			at(TModular(type, unit)) => type.isStrong()
		);
	}

	function isUncounted() {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.isUncounted(),
			at(TThis(decl is TypeDecl)) => decl.isUncounted(),
			at(TThis(_)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.some(ty -> ty.isUncounted()),
			at(TApplied(type, params)) => type.isUncounted(),
			at(TTypeVar(typevar)) => typevar.isUncounted(),
			at(TModular(type, unit)) => type.isUncounted()
		);
	}

	
	// Effects tracking

	function trackEffectsIn(ctx: Ctx): Null<Effects> {
		throw new haxe.exceptions.NotImplementedException();
	}

	function applyArgsTrackEffects(args: Array<Type>, ctx: Ctx): Null<Tuple2<Type, Effects>> {
		throw new haxe.exceptions.NotImplementedException();
	}

	
	// Privacy

	function canSeeMember(member: Member) {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.canSeeMember(member),
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
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.canSeeMethod(method),
			at(TThis(source)) => throw "todo",
			at(TBlank) => throw "bad",
			at(TMulti(types)) => types.every(ty -> ty.canSeeMethod(method)),
			at(TApplied(type, args)) => type.canSeeMethod(method),
			at(TTypeVar(typevar)) => throw "todo",
			at(TModular(type, unit)) => type.canSeeMethod(method)
		);
	}


	// Members

	function instMembers(from: AnyTypeDecl): Array<Member> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.instMembers(from),
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
				leastSpecific(types)
					.flatMap(type -> type.instMembers(from))
					.unique();
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


	// Method lookup

	function findSingleStatic(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findSingleStatic(ctx, name, from, getter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findSingleStatic(ctx, name, from, getter, cache);
					} else {
						//throw "???";
						td.findSingleStatic(ctx, name, from, getter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findSingleStatic(ctx, name, from, getter, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findSingleStatic(ctx, name, from, getter, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
				type.findSingleStatic(ctx, name, from, getter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findSingleStatic(ctx, name, from, getter, cache),
			at(TModular(type, unit)) => type.findSingleStatic(ctx, name, from, getter, cache)
		);
	}


	function findMultiStatic(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil): Array<MultiStaticKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findMultiStatic(ctx, names, from, setter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findMultiStatic(ctx, names, from, setter, cache);
					} else {
						//throw "???";
						td.findMultiStatic(ctx, names, from, setter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad "+span._and(s=>s.display()),
			at(TMulti(types)) => {
				leastSpecific(types)
					.flatMap(type -> type.findMultiStatic(ctx, names, from, setter, cache))
					.unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findMultiStatic(ctx, names, from, setter, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findMultiStatic(ctx, names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findMultiStatic(ctx, names, from, setter, cache),
			at(TModular(type, unit)) => type.findMultiStatic(ctx, names, from, setter, cache)
		);
	}


	function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findSingleInst(ctx, name, from, getter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findSingleInst(ctx, name, from, getter, cache);
					} else {
						//throw "???";
						td.findSingleInst(ctx, name, from, getter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findSingleInst(ctx, name, from, getter, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findSingleInst(ctx, name, from, getter, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
				type.findSingleInst(ctx, name, from, getter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findSingleInst(ctx, name, from, getter, cache),
			at(TModular(type, unit)) => type.findSingleInst(ctx, name, from, getter, cache)
		);
	}


	function findMultiInst(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil): Array<MultiInstKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findMultiInst(ctx, names, from, setter, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findMultiInst(ctx, names, from, setter, cache);
					} else {
						//throw "???"+names+from.fullName()+" "+td.fullName();
						td.findMultiInst(ctx, names, from, setter, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				types
					.flatMap(type -> type.findMultiInst(ctx, names, from, setter, cache))
					.unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				types
					.filter(type -> type.acceptsArgs(args))
					.flatMap(type -> type.findMultiInst(ctx, names, from, setter, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findMultiInst(ctx, names, from, setter, cache);
			},
			at(TTypeVar(typevar)) => typevar.findMultiInst(ctx, names, from, setter, cache),
			at(TModular(type, unit)) => type.findMultiInst(ctx, names, from, setter, cache)
		);
	}

	
	function findCast(ctx: Ctx, target: Type, from: AnyTypeDecl, cache: TypeCache = Nil): Array<CastKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findCast(ctx, target, from, cache),
			at(TInstance(decl, _, tctx)) => decl.findCast(ctx.innerTypevars(tctx), target, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td.thisType == from.thisType) {
						td.findCast(ctx, target, from, cache);
					} else {
						//throw "???"+td.fullName()+" "+from.fullName()+" "+target.fullName();
						td.findCast(ctx, target, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => leastSpecific(types).flatMap(t -> t.findCast(ctx, target, from, cache)),
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findCast(ctx, target, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findCast(ctx, target, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findCast(ctx, target, from, cache),
			at(TModular(type, unit)) => type.findCast(ctx, target, from, cache)
		);
	}


	function findUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Null<UnaryOpKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findUnaryOp(ctx, op, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findUnaryOp(ctx, op, from, cache);
					} else {
						//throw "???";
						td.findUnaryOp(ctx, op, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				final found = leastSpecific(types).filterMap(type -> type.findUnaryOp(ctx, op, from, cache)).unique();
				switch found {
					case []: null;
					case [kind]: kind;
					case kinds: throw "todo";
				}
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findUnaryOp(ctx, op, from, cache))
					.unique()
				._match(
					at([]) => null,
					at([kind]) => kind,
					at(kinds) => throw "todo"
				);
			},
			at(TApplied(type, args)) => {
				type.findUnaryOp(ctx, op, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findUnaryOp(ctx, op, from, cache),
			at(TModular(type, unit)) => type.findUnaryOp(ctx, op, from, cache)
		);
	}


	function findBinaryOp(ctx: Ctx, op: BinaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Array<BinaryOpKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo "+lookup+" "+lookup.span().display()+" "+this.span._and(s=>s.display()),
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findBinaryOp(ctx, op, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findBinaryOp(ctx, op, from, cache);
					} else {
						//throw "???";
						td.findBinaryOp(ctx, op, from, cache);
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types).flatMap(type -> type.findBinaryOp(ctx, op, from, cache)).unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findBinaryOp(ctx, op, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findBinaryOp(ctx, op, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findBinaryOp(ctx, op, from, cache),
			at(TModular(type, unit)) => type.findBinaryOp(ctx, op, from, cache)
		);
	}


	// Categories

	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findCategory(ctx, cat, forType, from, cache),
			at(TInstance(decl, _, tctx)) => decl.findCategory(ctx.innerTypevars(tctx), cat, forType, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findCategory(ctx, cat, forType, from, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).flatMap(t -> t.findCategory(ctx, cat, forType, from, cache)),
			at(TApplied({t: TMulti(types)}, args)) => {
				reduceOverloads(types.filter(type -> type.acceptsArgs(args)))
					.flatMap(type -> type.findCategory(ctx, cat, forType, from, cache))
					.unique();
			},
			at(TApplied(type, args)) => {
				type.findCategory(ctx, cat, forType, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findCategory(ctx, cat, forType, from, cache),
			at(TModular(type, unit)) => unit.findCategory(ctx, cat, forType, from, cache + type).concat(type.findCategory(ctx, cat, forType, from, cache + unit))
		);
	}


	function findThisCategory(ctx: Ctx, cat: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.findThisCategory(ctx, cat, from, cache),
			at(TInstance(decl, _, tctx)) => decl.findThisCategory(ctx.innerTypevars(tctx), cat, from, cache),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => {
					if(td == from) {
						td.findThisCategory(ctx, cat, from, cache);
					} else {
						throw "???";
					}
				},
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).flatMap(t -> t.findThisCategory(ctx, cat, from, cache)),
			at(TApplied(type, params)) => {
				type.findThisCategory(ctx, cat, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findThisCategory(ctx, cat, from, cache),
			at(TModular(type, unit)) => unit.findCategory(ctx, cat, type, from, cache + type).concat(type.findThisCategory(ctx, cat, from, cache + unit))
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
					at(d is AnyTypeDecl) => d,
					_ => throw "bad"
				), depth)._match(
					at(ty!) => {this.t=ty.t; ty;},
					_ => throw 'error: type `${this.fullName()}` does not exist! ${lookup.span().display()}'
				);
			},
			at(TApplied({t: TMulti(types)}, args)) => mostSpecific(types.filter(ty -> ty.acceptsArgs(args)))._match(
				at([]) => throw "bad",
				//at([{t: TConcrete(decl)}]) => ...
				at([ty]) => {t: TApplied(ty, args), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2)}, args), span: span}
			),
			//at(TApplied({t: TConcrete(decl)}, args)) => ...
			at(TApplied(ty, args)) => {t: TApplied(ty.simplify(), args.map(a -> a.simplify())), span: span},
			_ => this
		);
	}
}