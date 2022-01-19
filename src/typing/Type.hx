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
						} else if(d1.params.length == d2.params.length) {
							for(i => p in d1.params) {
								if(!p.hasChildType(d2.params[i])) {
									return true;
								}
							}
							return false;
						} else {
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

	function new(t: TypeKind, span: Null<Span>) {
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
			at(tvar is TypeVar) => {
				switch getFullPath(tvar.lookup).map(p -> '$p#${tvar.name.name}') {
					case p = Some(_): p;
					case None: Some(tvar.name.name);
				}
			},
			at(cat is Category) => {
				Some(cat.thisType.fullName()+"+"+cat.path.fullName());
			},
			_ => Some("???"+lookup)
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
			at(TPath(_, l, source)) => {
				//trace(l.span().display(), l.simpleName(), path.simpleName());
				this.t=this.simplify().t;
				this.findType(path, search, from, depth, cache);
			},
			at(TLookup(type, lookup, source)) => throw "NYI!",
			at(TConcrete(decl)) => decl.findType(path, search, from, depth, cache),
			at(TInstance(decl, _, tctx)) => decl.findType(path, search, from, depth, cache)._and(ty => ty.getInTCtx(tctx)),
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

	function maybeIn(ctx: Ctx): Null<Type> {
		t._match(
			at(TPath(_, _, _) | TLookup(_, _, _)) => {
				return this.simplify().maybeIn(ctx);
			},

			at(TConcrete({params: []})) => {
				return this;
			},
			at(TConcrete(decl)) => {
				ctx.where._match(
					at(WTypevars(tctx)) => {
						if(decl.typevars.allValues().every(tvar -> tctx.exists(tvar))) {
							final args = decl.params.map(p -> p.getIn(ctx));
							return decl.applyArgs(args);
						} else {
							return this;
						}
					},
					_ => return this
				);
			},
			
			at(TThis(_) | TBlank) => {
				return this;
			},

			at(TInstance(decl, params, tctx)) => {
				return {
					t: TInstance(
						decl,
						params.map(p -> p.getIn(ctx)),
						[for(k => v in tctx) k => v.getIn(ctx)]
					),
					span: span
				};
			},

			at(TMulti(types)) => {
				final types2 = [];

				for(type in types) {
					type.maybeIn(ctx)._and(ty => {
						types2.push(ty);
						// TODO: improve bc lazy and incomplete way of finding successfully parameterized HKT instances
						if(ty.t.match(TInstance(_, _, _)) && type.t.match(TConcrete(_))) {
							break;
						}
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

				for(type in types) type.t._match(
					at(TConcrete(decl)) => {
						if(decl.acceptsArgs(args2)) decl.applyArgs(args2)._and(type2 => {
							types2.push(type2);
						});
					},
					_ => type.maybeIn(ctx)._and(type2 => {
						if(type2.acceptsArgs(args2)) type2.applyArgs(args2)._and(type3 => {
							types2.push(type3);
						});
					})
				);

				return mostSpecific(types2)._match(
					at([]) => null,
					at([type]) => type,
					_ => {t: TMulti(types2), span: span}
				);
			},

			at(TApplied({t: TConcrete(decl)}, args)) => {
				return decl.applyArgs(args.map(a -> a.getIn(ctx)));
			},
			at(TApplied(type, args)) => {
				return type.maybeIn(ctx)._and(type2 => {
					type2.applyArgs(args.map(a -> a.getIn(ctx)));
				});
			},

			at(TTypeVar(typevar)) => {
				return ctx.findTypevar(typevar)._or(
					this
				);
			},

			at(TModular(type, _)) => {
				return type.maybeIn(ctx);
			}
		);
	}
	
	function getIn(ctx: Ctx): Type {
		return this.maybeIn(ctx)._andOr(
			ty => ty,
			throw 'Error: invalid type `${this.fullName()}`!'
		);
	}

	function getInTCtx(ctx: TypeVarCtx): Type {
		return this.maybeIn({where: WTypevars(ctx), thisType: this})._andOr(
			ty => ty,
			throw 'Error: invalid type `${this.fullName()} ${this.span.display()}`!'
		);
	}


	function maybeFrom(type: Type): Null<Type> {
		return t._match(
			at(TPath(_, _, _) | TLookup(_, _, _)) => {
				this.simplify().maybeFrom(type);
			},

			/*at(TConcrete({params: [], type: ty} is DirectAlias)) => {
				ty.maybeFrom(type);
			},*/
			at(TConcrete(_) | TBlank) => {
				this;
			},

			at(TInstance(decl, params, ctx)) => type.t._match(
				at(TConcrete({params: [], type: ty} is DirectAlias)) => {
					this.maybeFrom(ty.simplify());
				},
				at(TConcrete({params: [], type: ty} is StrongAlias)) => {
					this.maybeFrom(ty.simplify());
				},
				at(TThis(source)) => this.maybeFrom(source.thisType),
				at(TModular(ty, _)) => this.maybeFrom(ty),
				_ => {
					{
						t: TInstance(
							decl,
							params.map(p -> p.getFrom(type)),
							[for(k => v in ctx) k => v.getFrom(type).getInTCtx(ctx)]
						),
						span: span
					};
				}
			),

			/*at(TThis({params: [], type: ty} is DirectAlias)) => {
				ty.maybeFrom(type);
			},*/
			at(TThis(decl is TypeDecl)) => {
				// TODO: check if type <: decl?
				return if(decl.hasChildType(type) || type.hasParentDecl(decl)
				|| (decl == Pass2.STD_MultiKind && type.isFlags())
				|| (decl == Pass2.STD_Value)) {
					type;
				} else {
					{t: decl.thisType.t, span: this.span};
				}
			},
			at(TThis(decl is TypeDecl)) => {
				if(decl.hasChildType(type) || type.hasParentDecl(decl)) {
					return type;
				} else {
					throw {t: decl.thisType.t, span: span};
				}
			},
			at(TThis(_)) => throw "todo",

			at(TMulti(types)) => {
				final types2 = [];

				for(type0 in types) {
					type0.maybeFrom(type)._and(ty => {
						types2.push(ty);
					});
				}

				types2._match(
					at([]) => null,
					at([type0]) => type0,
					_ => {t: TMulti(types2), span: span}
				);
			},

			at(TApplied({t: TMulti(types)}, args)) => {
				final types2 = [];
				final args2 = args.map(a -> a.getFrom(type));

				for(type0 in types) {
					type0.maybeFrom(type)._and(type2 => {
						if(type2.acceptsArgs(args2)) type2.applyArgs(args2)._and(type3 => {
							types2.push(type3);
						});
					});
				}

				mostSpecific(types2)._match(
					at([]) => null,
					at([type0]) => type0,
					_ => {t: TMulti(types2), span: span}
				);
			},

			at(TApplied({t: TThis(decl)}, args)) => {
				// TODO: check if type <: decl?
				args = args.map(a -> a.getFrom(type));
				type.t._match(
					at(TConcrete(decl2)) => decl2.applyArgs(args),
					at(TInstance(decl2, _, _)) => decl2.applyArgs(args),
					at(TThis(decl2)) => decl2.applyArgs(args)._or(decl.applyArgs(args)),
					at(TApplied(type2, _)) => type2.applyArgs(args),
					at(TMulti(types)) => {
						// WHY ARE THERE RANDOM DUP TYPE INSTANCES?!?!?!?!?!?!?!
						for(ty in types) ty.t._match(
							at(TInstance(decl, params, _)) => {
								if(decl.acceptsArgs(params) && decl.applyArgs(params) != null) {
									return ty;
								}
							},
							_ => throw "???????????????"
						);
						throw "???????????????";
					},
					_ => throw "todo??? "+type.fullName()+" "+args[0].span.display()
				);
			},
			
			at(TApplied(type0, args)) => {
				type0.maybeFrom(type)._and(type2 => {
					type2.applyArgs(args.map(a -> a.getFrom(type)));
				});
			},

			at(TTypeVar(typevar)) => return type.t._match(
				at(TInstance(decl, params, ctx)) => {
					ctx[typevar]._or(
						this
					);
				},
				at(TConcrete({params: [], type: ty} is DirectAlias)) => {
					this.maybeFrom(ty.simplify().getFrom(type));
				},
				at(TConcrete({params: [], type: ty} is StrongAlias)) => {
					this.maybeFrom(ty.simplify().getFrom(type));
				},
				at(TThis(source)) => this.maybeFrom(source.thisType),
				at(TModular(ty, _)) => this.maybeFrom(ty),
				_ => {
					this;
				}
			),

			at(TModular(type0, _)) => {
				type0.maybeFrom(type);
			}
		);
	}
	
	function getFrom(type: Type): Type {
		return this.maybeFrom(type)._andOr(
			ty => ty,
			throw 'Error: invalid type `${this.fullName()}` in `${type.fullName()}`! '+span.display()
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
			at(TBlank) => true, // ?????????????????
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentDecl(decl)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).every(ty -> ty.hasParentDecl(decl)),
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
			at(TBlank) => false, // ???????????????????
			at(TMulti(types)) => leastSpecific(types).every(ty -> ty.hasChildDecl(decl)),
			at(TApplied({t: TMulti(types)}, args)) => leastSpecific(types.filter(ty -> ty.acceptsArgs(args))).every(ty -> ty.hasChildDecl(decl)),
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
			at(TConcrete(decl)) => decl.hasParentType(type),
			at(TInstance(decl, params, _)) => {
				// TODO
				/*type.t._match(
					at(TInstance(decl2, params2, _)) => {
						if(decl==decl2) trace(decl.fullName(),params.map(p->p.fullName()),params2.map(p->p.fullName()));
					},
					_ => {}
				);*/
				type.t._match(
					at(TInstance(decl2, params2, ctx2)) => {
						if(decl.hasParentDecl(decl2)) {
							if(decl==decl2) {
								params.every2(params2, (p1, p2) -> p1.hasParentType(p2));
							} else {
								// TODO
								true;
							}
						} else {
							false;
						}
					},
					_ => decl.hasParentType(type)
				);
			},
			at(TThis(source)) => source.hasParentType(type),
			at(TBlank) => true, // ?????????????????
			at(TMulti(types)) => reduceOverloads(types).every(ty -> ty.hasParentType(type)),
			at(TApplied({t: TMulti(types)}, args)) => types.filter(ty -> ty.acceptsArgs(args)).every(ty -> ty.hasParentType(type)),
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
			at(TConcrete(decl)) => decl.hasChildType(type),
			at(TInstance(decl, params, _)) => {
				decl.hasChildType(type); // TODO
			},
			at(TThis(source)) => source.hasChildType(type),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => leastSpecific(types).every(ty -> ty.hasChildType(type)),
			at(TApplied({t: TMulti(types)}, args)) => leastSpecific(types.filter(ty -> ty.acceptsArgs(args))).every(ty -> ty.hasChildType(type)),
			at(TApplied(type2, args)) => {
				type2.acceptsArgs(args) && (type2.hasChildType(type) || type.t._match(
					at(TApplied(ty, ps), when(args.length == ps.length && ty.acceptsArgs(ps))) => {
						if(type2.t == ty.t
						|| Util._match([type2.t, ty.t],
							at([TThis(d1) | TConcrete(d1), TThis(d2) | TConcrete(d2)]) => (d1.lookup == d2.lookup && d1.name.name == d2.name.name),
							_ => false
						)
						|| type2.hasChildType(ty)) {
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

	function unifyWithType(type: Type): Null<Type> {
		return this.t == type.t ? this : Util._match([this.t, type.t],
			at([TConcrete(d1), TConcrete(d2)]) => if(d1 == d2) this else null,
			at([TConcrete(decl1), TInstance(decl2, params2, _)]) => {
				if(decl2.hasParentDecl(decl1)) {
					type;
				} else {
					null;
				}
			},
			at([TMulti(types), TInstance(decl2, params2, _)]) => {
				if(types.some(ty -> decl2.hasParentType(ty))) {
					type;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TConcrete(decl2)]) => {
				if(decl1.hasParentDecl(decl2)) {
					this;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TMulti(types)]) => {
				if(types.some(ty -> decl1.hasParentType(ty))) {
					this;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TInstance(decl2, params2, _)]) => {
				if(decl1 == decl2
				&& params1.every2Strict(params2, (p1, p2) -> p1.unifyWithType(p2) != null)) this else null;
			},
			at([TModular(t1, _), _]) => t1.unifyWithType(type),
			at([_, TModular(t2, _)]) => this.unifyWithType(t2),
			at([TApplied(t1, a1), TApplied(t2, a2)], when(a1.length == a2.length)) => {
				t1.unifyWithType(t2)._and(t => {
					{t: TApplied(t, [for(i => a1_ in a1) {
						a1_.unifyWithType(a2[i])._match(
							at(a!) => a,
							_ => return null
						);
					}]), span: t.span._or(t1.span)};
				});
			},
			at([TMulti(types1), TMulti(types2)]) => {
				// TODO: intersection, not ==
				if(types1.length == types2.length && types1.sorted((a, b) -> a.__compare(b)).every2(types2.sorted((a, b) -> a.__compare(b)), (t1, t2) -> t1.unifyWithType(t2) != null)) {
					this;
				} else {
					null;
				}
			},
			at([TTypeVar(tv1), TTypeVar(tv2)]) => {
				if(tv1.strictUnifyWithTypevar(tv2)) { // TODO: unifyWithTypevar
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

	function strictUnifyWithType(type: Type): Null<Type> {
		return this.t == type.t ? this : Util._match([this.t, type.t],
			at([TConcrete({type: a, params: []} is DirectAlias), _]) => a.strictUnifyWithType(type),
			at([_, TConcrete({type: a, params: []} is DirectAlias)]) => this.strictUnifyWithType(a),
			
			at([TConcrete(d1), TConcrete(d2)]) => if(d1 == d2) this else null,
			at([TConcrete(decl1), TInstance(decl2, params2, _)]) => {
				if(decl2 == decl1) {
					type;
				} else {
					null;
				}
			},
			at([TMulti(types), TInstance(decl2, params2, _)]) => {
				if(types.some(ty -> type.strictUnifyWithType(ty) == type)) {
					type;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TConcrete(decl2)]) => {
				if(decl1 == decl2) {
					this;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TMulti(types)]) => {
				if(types.some(ty -> this.strictUnifyWithType(ty) == this)) {
					this;
				} else {
					null;
				}
			},
			at([TInstance(decl1, params1, _), TInstance(decl2, params2, _)]) => {
				if(decl1 == decl2
				&& params1.every2Strict(params2, (p1, p2) -> p1.strictUnifyWithType(p2) != null)) this else null;
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
					}]), span: t.span._or(t1.span)};
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
			at(TConcrete(decl)) => decl.acceptsArgs(args),
			at(TInstance(decl2, params, _)) => false,//throw "todo "+this.fullName()+" "+span._and(s=>s.display()),
			at(TThis(source)) => source._match(
				at(decl is TypeDecl) => decl.params.every2Strict(args, (p, a) -> a.hasParentType(p)),
				at(tvar is TypeVar) => tvar.params.every2Strict(args, (p, a) -> a.hasParentType(p)),
				_ => throw "bad"
			),
			at(TBlank) => true,
			at(TMulti(types)) => {
				reduceOverloads(types).every(type -> type.acceptsArgs(args));
			},
			at(TApplied(type, args2)) => {
				if(args2.count(a -> a.hasTypevars()) == args.length) {
					throw "todo "+this.fullName()+" "+args.map(a->a.fullName())+" "+args[0].span.display();
				} else {
					false;
				}
			},
			at(TTypeVar(tvar)) => tvar.params.every2Strict(args, (p, a) -> p.hasChildType(a)),
			at(TModular(type, _)) => type.acceptsArgs(args)
		);
	}

	function applyArgs(args: Array<Type>): Null<Type> {
		return t._match(
			at(TPath(_, _, _)) => throw "bad",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => {
				/*if(!decl.acceptsArgs(args))return null;
				if(args.map(a->a.fullName()).join("").contains("Star.Core.Array.[Parser.Type]")) {
					trace(args);
				}*/
				var res = decl.applyArgs(args);
				res._and(r => {
					r.t._match(
						at(TInstance(decl2, params, tctx)) => {
							if(decl2.params.some(p -> p.hasTypevars()) && tctx.size() == 0) {
								//trace(r.fullName());
								//return null;
							}
						},
						_ => {}
					);
					if(r.span == null) res = {t: r.t, span: span};
				});
				res;
			},
			at(TInstance(_, _, _)) => { // partially-applied type (?)
				trace("todo"+this.fullName());
				return null;
			},
			at(TThis(_) | TBlank) => {t: TApplied(this, args), span: span},
			at(TMulti(types)) => {
				var types2 = types.filterMap(ty -> ty.acceptsArgs(args) ? ty.applyArgs(args) : null);

				final n = types2.count(ty -> ty.hasTypevars());
				if(n != 0 && n != types2.length) {
					// ...
					types2=types2.filter(ty->!ty.hasTypevars());
				} else if(n == 0) {
					//trace(types2.map(t->t.fullName()));
					types2=mostSpecific(types2);
				}

				types2._match(
					at([]) => null,
					at([ty]) => ty,
					at(tys) => {t: TMulti(tys), span: span}
				);
			},
			at(TApplied({t: TThis(decl)}, args2)) => {
				//trace("todo "+span._and(s=>s.display()));
				var res = decl.applyArgs(args);
				res._and(r => if(r.span == null) res = {t: r.t, span: span});
				res;
			},
			at(TApplied(type, params)) => throw "todo"+this.fullName()+" "+args, // partially-applied type
			at(TTypeVar(typevar)) => throw "todo", // higher-kinded type
			at(TModular(type, _)) => {
				var res = type.applyArgs(args);
				res._and(r => if(r.span == null) res = {t: r.t, span: span});
				res;
			}
		);
	}


	// Binding

	function bindTo(onto: Type, ctx: TypeVarCtx): Null<Type> {
		return Util._match([this.t, onto.t],
			at([TPath(_, _, _), _] | [TLookup(_, _, _), _]) => this.simplify().bindTo(onto, ctx),
			at([_, TPath(_, _, _)] | [_, TLookup(_, _, _)]) => this.bindTo(onto.simplify(), ctx),

			at([TModular(t1, _), _]) => t1.bindTo(onto, ctx),
			at([_, TModular(t2, _)]) => this.bindTo(t2, ctx),

			at([_, TBlank]) => this,
			//at([TBlank, _]) => onto,

			at([TThis(decl1), TThis(decl2)]) => {
				if(decl1.hasParentType(decl2.thisType) || decl2.hasChildType(decl1.thisType)) {
					this;
				} else {
					null;
				}
			},
			at([TThis(decl), TTypeVar(typevar)]) => {
				ctx[typevar]._match(
					at(type!) => this.strictUnifyWithType(type)._or(decl.thisType.strictUnifyWithType(type)),
					_ => {
						ctx[typevar] = this;
						this;
					}
				);
			},
			at([TThis(decl), _]) => {
				if(decl.hasParentType(onto) || onto.hasChildType(decl.thisType)) {
					this;
				} else {
					null;
				}
			},

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
				if(decl1.hasParentDecl(decl2)||decl2.hasChildDecl(decl1)) {
					this;
				} else {
					null;
				}
			},

			// TODO: account for generic typevars and HKTs
			at([TConcrete(decl), TApplied(base, _)]) => {
				null;
			},

			at([TConcrete(decl1), TInstance(decl2, params, tctx)]) => {
				if(decl1 == decl2) {
					onto;
				} else {
					null;
				}
			},

			// TODO: account for generic typevars and HKTs
			at([_, TConcrete(decl)]) => {
				if(this.hasParentDecl(decl)) {
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

			at([TApplied(_, _), TInstance(_, _, _)]) => {
				this.simplify()._match(
					at({t: TApplied({t: TMulti(_)}, _)}) => throw "todo",
					at(ty) => ty.bindTo(onto, ctx)
				);
			},

			at([TInstance(decl, params, tctx), TApplied(base, args)]) => {
				// TODO: make this actually work with subtypes/supertypes
				if(decl.hasParentType(base) && base.hasChildDecl(decl)) {
					final params2 = params.zip(args, (p, a) -> p.bindTo(a, ctx));
					var res = decl.applyArgs(params2);
					res._and(r => if(r.span == null) res = {t: r.t, span: span});
					res;
					
					//throw "todo "+this.fullName()+" "+onto.fullName()+" "+span._and(s=>s.display());
				} else {
					null;
				}
			},

			at([TInstance(decl1, params1, tctx1), TInstance(decl2, params2, tctx2)]) => {
				if((decl1 == decl2 || decl1.hasRefinementDecl(decl2)) && params1.length == params2.length) {
					final tctx: TypeVarCtx = [];
					final params = [];
					params1._for(i => param1, {
						param1.bindTo(params2[i], tctx)._match(
							at(param!) => params.push(param),
							_ => return null
						);
					});
					for(tv => rt in tctx) {
						ctx[tv] = rt;
					}
					/*trace("");
					trace(tctx1.display());trace(tctx2.display());trace(tctx.display());
					trace("");*/
					decl1.applyArgs(params)._and(ty => {
						ty.span = span;
						ty;
					});
				} else {
					if(decl1.hasParentDecl(decl2)) {
						/*if(decl1.name.name=="Func") {
							trace(cast(decl1, Namespace).parents.filterMap(p->p.bindTo(onto, ctx.copy())).map(p->p.getFrom(this).fullName()));//.getFrom(this)
						}*/
						decl1._match(
							// maybe use Protocol?
							at(ns is Namespace) => {
								ns.parents.findMap(p -> p.bindTo(onto, ctx))._and(res => res.getFrom(this));
							},
							at(da is DirectAlias) => {
								da.type.getInTCtx(tctx1).bindTo(onto, ctx);
							},
							_ => throw "todo"
						);
					} else {
						null;
					}
				};
			},
			
			at([TInstance(decl, params, tctx), TMulti(types)]) => {
				types.filterMap(ty -> this.bindTo(ty, ctx))._match(
					at([]) => null,
					at([ty]) => ty,
					at(tys) => {t: TMulti(tys), span: onto.span}
				);
			},

			at([TMulti(types), TInstance(decl, params, tctx)]) => {
				// lazy for now
				if(types.some(ty -> ty.hasParentType(onto))) {
					onto;
				} else {
					null;
				}
			},

			at([TMulti(types), TApplied(base, args)]) => {
				// lazy for now
				base.applyArgs(args)._match(
					at(onto2!) => this.bindTo(onto2, ctx),
					_ => null
				);
			},

			at([_, TMulti(types)]) => {
				if(leastSpecific(types).every(ty -> this.hasParentType(ty) && ty.hasChildType(this))) {
					throw "todo "+this.fullName()+" "+onto.fullName();
				} else {
					null;
				}
			},

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


	// Iterating

	function iterElemType(): Null<Type> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.iterElemType(),
			at(TInstance(decl, _, tctx)) => decl.iterElemType(),
			at(TThis(decl)) => decl.iterElemType(),
			at(TBlank) => return null,
			at(TMulti(types)) => {
				leastSpecific(types)._match(
					at([]) => null,
					at([ty]) => ty.iterElemType(),
					at(tys) => throw "todo"
				);
			},
			at(TApplied(type, params)) => type.applyArgs(params)._and(ty => {
				Util._and(ty.iterElemType(), et => et.getFrom(ty));
			}),
			at(TTypeVar(typevar)) => typevar.iterElemType(),
			at(TModular(type, unit)) => type.iterElemType()
		)._and(ty => ty.getFrom(this));
	}

	function iterAssocType(): Null<Tuple2<Type, Type>> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl)) => decl.iterAssocType(),
			at(TInstance(decl, _, tctx)) => decl.iterAssocType(),
			at(TThis(decl)) => decl.iterAssocType(),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types)._match(
					at([]) => null,
					at([ty]) => ty.iterAssocType(),
					at(tys) => throw "todo"
				);
			},
			at(TApplied(type, params)) => type.applyArgs(params)._and(ty => {
				ty.iterAssocType()._match(
					at(null) => null,
					at({_1: k, _2: v}) => new Tuple2(k.getFrom(ty), v.getFrom(ty))
				);
			}),
			at(TTypeVar(typevar)) => typevar.iterAssocType(),
			at(TModular(type, unit)) => type.iterAssocType()
		)._match(
			at(null) => null,
			at({_1: k, _2: v}) => {_1: k.getFrom(this), _2: v.getFrom(this)}
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
			at(TTypeVar(typevar)) => {
				typevar.instMembers(from);
			},
			at(TModular(type, unit)) => type.instMembers(from)
		);
	}

	function findInstMember(ctx: Ctx, name: String, allowStatic = true, onlyParents = false): Null<MemberKind> {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TConcrete(decl) | TInstance(decl, _, _)) => decl.findInstMember(ctx, name, allowStatic, onlyParents),
			at(TThis(source)) => source._match(
				at(td is TypeDecl) => td.findInstMember(ctx, name, allowStatic, onlyParents),
				_ => throw "todo"
			),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => {
				leastSpecific(types)
					.filterMap(type -> type.findInstMember(ctx, name, allowStatic, onlyParents))
					.unique()
					._match(
						at([]) => null,
						at([m]) => m,
						at(ms) => throw "todo"
					);
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				leastSpecific(types.filter(type -> type.acceptsArgs(args)))
					.filterMap(type -> type.findInstMember(ctx, name, allowStatic, onlyParents))
					.unique()
					._match(
						at([]) => null,
						at([m]) => m,
						at(ms) => throw "todo"
					);
			},
			at(TApplied(type, args)) => {
				type.findInstMember(ctx, name, allowStatic, onlyParents);
			},
			at(TTypeVar(typevar)) => {
				typevar.findInstMember(ctx, name, allowStatic, onlyParents);
			},
			at(TModular(type, unit)) => type.findInstMember(ctx, name, allowStatic, onlyParents)
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
				types
					.flatMap(type -> type.findMultiStatic(ctx, names, from, setter, cache))
					.unique();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				types
					.filter(type -> type.acceptsArgs(args))
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
			at(TThis(source)) => source.findSingleInst(ctx, name, from, getter, cache),
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
			at(TThis(source)) => source.findMultiInst(ctx, names, from, setter, cache),
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
			at(TThis(source)) => source.findCast(ctx, target, from, cache),
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
			at(TThis(source)) => source.findUnaryOp(ctx, op, from, cache),
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
			at(TThis(source)) => source.findBinaryOp(ctx, op, from, cache),
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
			at(TThis(source)) => source.findCategory(ctx, cat, forType, from, cache),
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
			at(TThis(source)) => source.findThisCategory(ctx, cat, from, cache),
			at(TBlank) => throw "bad",
			at(TMulti(types)) => reduceOverloads(types).flatMap(t -> t.findThisCategory(ctx, cat, from, cache)),
			at(TApplied(type, args)) => {
				type.findThisCategory(ctx, cat, from, cache);
			},
			at(TTypeVar(typevar)) => typevar.findThisCategory(ctx, cat, from, cache),
			at(TModular(type, unit)) => unit.findCategory(ctx, cat, type, from, cache + type).concat(type.findThisCategory(ctx, cat, from, cache + unit))
		);
	}


	function hasTypevars(): Bool {
		return t._match(
			at(TPath(depth, lookup, source)) => throw "todo",
			at(TLookup(type, lookup, source)) => throw "todo",
			at(TInstance(_, params, _)) => params.some(p -> p.hasTypevars()),
			at(TMulti(types)) => types.some(ty -> ty.hasTypevars()),
			at(TApplied(type, args)) => type.hasTypevars() || args.some(a -> a.hasTypevars()),
			at(TTypeVar(_)) => true,
			_ => false
		);
	}


	function getMostSpecific(): Type {
		return t._match(
			at(TModular(type, unit)) => {t: TModular(type.getMostSpecific(), unit), span: span},
			at(TMulti(types)) => mostSpecific(types)._match(
				at([]) => throw "bad",
				at([type]) => type,
				at(types2) => if(types2.length == types.length) this else {t: TMulti(types2), span: span}
			),
			at(TApplied({t: TMulti(types)}, args)) => mostSpecific(types.filter(ty -> ty.acceptsArgs(args)).map(ty -> ty.getMostSpecific()))._match(
				at([]) => throw "bad",
				at([ty]) => {t: TApplied(ty, args), span: span},
				at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2), span: span}, args), span: span}
			),
			_ => this
		);
	}

	function getLeastSpecific(): Type {
		return t._match(
			at(TModular(type, unit)) => {t: TModular(type.getLeastSpecific(), unit), span: span},
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
					at(types2) => if(types2.length == types.length) this else {t: TApplied({t: TMulti(types2), span: span}, args), span: span}
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
					at(ty!) => {this.t=ty.t; ty.simplify();},
					_ => throw 'error: type `${this.fullName()}` does not exist! ${lookup.span().display()}'
				);
			},
			// HACKY BC IT'S DUMB PLS JUST WORK
			at(TConcrete({type: type, params: []} is DirectAlias)) => {
				if(type.hasTypevars()) {
					this;
				} else {
					type.getFrom(this);
				}
			},
			at(TApplied({t: TModular(type, _)}, args)) => {
				({t: TApplied(type, args), span: span}:Type).simplify();
			},
			at(TApplied({t: TMulti(types)}, args)) => {
				final args2 = args.map(a -> a.simplify());
				final types2=types.filter(ty->ty.acceptsArgs(args2)&&ty.applyArgs(args2)!=null);
				/*({
					final res = types.filter(ty -> ty.acceptsArgs(args));
					if(args.some(a -> a.hasTypevars())) {
						res;
					} else {
						trace(types.map(t->t.fullName()+t.acceptsArgs(args)), args.map(a->a.fullName()));
						trace("@@");
						trace("args:    ", args.map(a->a.fullName()));
						trace("input:   ", res.map(r->r.fullName()));
						trace("accepts: ", res.filter(r->r.acceptsArgs(args)).map(r->r.fullName()));
						trace("apply:   ", res.filter(r->r.applyArgs(args)!=null).map(r->r.fullName()));
						trace("\n");
						mostSpecific(res.filter(r->r.applyArgs(args)!=null));
					}
				})*/types2._match(
					at([]) => throw "bad",
					at([{t: TConcrete(decl)}]) => decl.applyArgs(args)._match(
						at(res!) => { res.span = span; res; },
						_ => throw 'error: type `${decl.fullName()}` does not accept provided arguments [${args.joinMap(", ", a -> a.fullName())}] ${span._and(s=>s.display())}'
					),
					at([ty]) => {t: TApplied(ty, args), span: span},
					_ => if(types2.length == types.length && args.equals(args2)) this else {t: TApplied({t: TMulti(types2), span: span}, args), span: span}
				);
			},
			at(TApplied({t: TConcrete(decl)}, args)) => {
				args = args.map(a -> a.simplify());
				if(!decl.acceptsArgs(args)) return null;
				decl.applyArgs(args)._match(
					at(res!) => { res.span = span; res; },
					_ => throw 'error: type `${decl.fullName()}` does not accept provided arguments [${args.joinMap(", ", a -> a.fullName())}] ${span._and(s=>s.display())}'
				);
			},
			at(TApplied(ty, args)) => {t: TApplied(ty.simplify(), args.map(a -> a.simplify())), span: span},
			_ => this
		);
	}
}