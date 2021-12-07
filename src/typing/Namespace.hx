package typing;

import typing.Traits;

abstract class Namespace extends TypeDecl {
	final parents: Array<Type> = [];
	@ignore final decls = new MultiMap<String, TypeDecl>();
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	var staticInit: Option<StaticInit> = None;
	var staticDeinit: Option<StaticDeinit> = None;
	var sealed: Option<Option<Type>> = None;
	final categories: Array<Category> = [];


	inline function addTypeDecl(decl: TypeDecl) {
		decls.add(decl.name.name, decl);
	}

	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		//if(cache.contains(this)) return None;
		//cache += thisType;

		if(from == null) from = this;

		return path._match(
			at([[span, "This", args]], when(search != Inside && depth == 0)) => {
				if(args.length == 0) {
					{t: TThis(this), span: span};
				} else {
					// errors prob shouldn't be attatched to *this* type decl, but eh
					if(params.length == 0) {
						errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
						null;
					} else if(args.length > params.length) {
						errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
						null;
					} else if(args.length < params.length) {
						errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
						null;
					} else {
						{t: TApplied({t: TThis(this), span: span}, args), span: span};
					}
				}
			},
			at([[span, typeName, args], ...rest]) => {
				var finished = true;
				final res: Null<Type> = (if(search == Inside || typevars.size == 0) {
					cast decls.find(typeName);
				} else {
					final ds: Option<Array<AnyFullTypeDecl>> = cast decls.find(typeName);
					final tvs: Option<Array<AnyFullTypeDecl>> = cast typevars.find(typeName);
					
					tvs.orElseDo([]).concat(ds.orElseDo([]))._match(
						at([]) => None,
						at(ts) => Some(ts)
					);
				}).map(found -> found.filter(decl ->
					!cache.contains(decl.thisType)
					&& (
						(
							(decl is TypeVar && decl.params.length == 0)
							|| (decl is TypeDecl && args.length == 0)
						)
						|| decl.params.length == args.length
					)
				))._match(
					at(None | Some([])) => if(search == Inside) null else lookup.findType(path, Outside, from, depth, cache),
					at(Some(_), when(depth != 0)) => {
						if(search == Inside) {
							null;
						} else {
							lookup.findType(path, Outside, from, depth - 1, cache);
						}
					},
					at(Some([type])) => switch [args, type.params] {
						case [[], []]:
							finished = false;
							{t: type.thisType.t, span: span};
						case [[], _]:
							finished = false;
							{t: type.thisType.t, span: span}; // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							if(search == Inside) {
								errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
								null;
							} else {
								// error...?
								lookup.findType(path, Outside, from, depth, cache);
							}
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								null;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								null;
							} else {
								finished = false;
								{t: TApplied(type.thisType, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
							}
					},
					at(Some(found)) => {
						if(args.length == 0) {
							finished = false;
							{t: TMulti(found.map(t -> t.thisType)), span: span};
						} else switch found.filter(t -> t.params.length == args.length).map(t -> t.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								null;
							case [type]:
								finished = false;
								{t: TApplied(type, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
							case types:
								finished = false;
								{t: TApplied({t: TMulti(types), span: span}, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
						}
					}
				);
				
				Util._match([rest, res],
					at([_, null]) => lookup.findType(path, Outside, from, depth, cache),
					at([_, _], when(finished)) => res,
					at([Nil3, _]) => res,
					at([_, {t: TConcrete(decl)}]) => decl.findType(rest, Inside, from, 0, cache),
					at([_, type!!]) => {t: TLookup(type, rest, this), span: span}
				);
			},
			_ => throw "bad"
		);
	}

	override function hasErrors() {
		return super.hasErrors()
			|| decls.allValues().some(d -> d.hasErrors())
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| categories.some(c -> c.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(decl in decls) result = result.concat(decl.allErrors());
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(category in categories) result = result.concat(category.allErrors());

		return result;
	}


	// Type checking

	override function hasParentDecl(decl: TypeDecl) {
		return this == decl
			|| parents.some(p -> p.hasParentDecl(decl) || p.t._match(
				at(TConcrete(pd) | TApplied({t: TConcrete(pd)}, _)) =>
					pd.refinements.some(r -> this.params.every2Strict(r.params, (p1, p2) -> p1.hasParentType(p2))),
				_ => false
			))
			|| super.hasParentDecl(decl);
	}

	override function hasParentType(type: Type) {
		return super.hasParentType(type)
			|| parents.some(p -> p.hasParentType(type) || type.t._match(
				at(TConcrete(pd)) => {
					//trace(type.fullName(),p,pd,this);
					pd.refinements.some(r -> this.params.every2Strict(r.params, (p1, p2) -> p1.hasParentType(p2)));
				},
				at(TApplied({t: TConcrete(pd)}, args)) => {
					//trace(this.fullName(), pd.fullName(), type.fullName(),pd.thisType.acceptsArgs(args),this);
					pd.refinements.some(r -> this.params.every2Strict(r.params, (p1, p2) -> p1.hasParentType(p2)));
				},
				_ => false
			));
	}


	override function hasChildDecl(decl: TypeDecl) {
		return this == decl
			|| refinements.some(r -> r == decl || r.hasChildDecl(decl)
				|| (r.params.length != 0 && r.params.length == decl.params.length && {
					r.params.every2(decl.params, (p1, p2) -> p1.hasChildType(p2));
				}))
			|| decl._match(
				at(ns is Namespace) => {
					//final l = this.fullName();
					//final r = ns.fullName();
					//if(!(l.containsAny(["Comparable", "Iterable"]) || r.containsAny(["Comparable", "Iterable"])))trace(l,r,ns.parents.map(p->p.fullName()));
					ns.parents.some(p -> /*this.hasChildType(p)*/p.t._match(
						at(TConcrete(pd) | TApplied({t: TConcrete(pd)}, _)) =>
							pd.refinements.some(r -> this.params.every2Strict(r.params, (p1, p2) -> p1.hasChildType(p2))),
						_ => false
					));
				},
				at(da is DirectAlias) => this.hasChildType(da.type),
				at(sa is StrongAlias) => {
					!sa.noInherit && this.hasChildType(sa.type);
				},
				_ => false
			)
			|| super.hasChildDecl(decl);
	}

	override function hasChildType(type: Type) {
		return super.hasChildType(type)
			|| type.hasParentDecl(this);
	}


	// Attributes

	override function isNative(kind: NativeKind) {
		return parents.some(p -> p.isNative(kind));
	}


	// Privacy

	override function canSeeMember(member: Member) {
		return super.canSeeMember(member)
			|| parents.some(p -> p.canSeeMember(member));
	}
	
	override function canSeeMethod(method: AnyMethod) {
		return super.canSeeMethod(method)
			|| parents.some(p -> p.canSeeMethod(method));
	}


	// Members

	override function instMembers(from: AnyTypeDecl) {
		return staticMembers.filter(mem -> from.canSeeMember(mem))
			.concat(super.instMembers(from));
	}


	// Method lookup

	function defaultSingleStatic(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false): Null<SingleStaticKind> {
		return null;
	}

	override function findSingleStatic(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		for(mem in staticMembers) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SSMember(mem);
			}
		}

		for(mth in staticMethods) mth._match(
			at(sm is SingleStaticMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SSMethod(sm);
				}
			},
			at(mm is MultiStaticMethod) => if(!getter) {
				if(mm.params[0].label.name == name && mm.params.every(p -> p.value != null)) {
					return SSMultiMethod(mm);
				}
			},
			_ => {}
		);
		
		for(parent in parents) {
			parent.findSingleStatic(ctx, name, from, getter, cache)._match(
				at(ss!) => return ss,
				_ => {}
			);
		}

		for(refinee in refinees) {
			refinee.findSingleStatic(ctx, name, from, getter, cache)._match(
				at(ss!) => return ss,
				_ => {}
			);
		}

		return cache.list._match(
			at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)} is Type, ..._]) => defaultSingleStatic(ctx, name, from, getter),
			_ => null
		);
	}


	override function findMultiStatic(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiStaticKind> = [];

		names._match(at([name]) => for(mem in staticMembers) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				candidates.push(MSMember(mem));
			}
		}, _ => {});

		if(setter) {
			throw "todo";
		} else {
			for(mth in staticMethods) mth._match(
				at(mm is MultiStaticMethod) => if(from.canSeeMethod(mm))
					mm.params.matchesNames(names)._match(
						at(Yes) => candidates.push(MSMethod(mm)),
						at(Partial(indexes)) => candidates.push(MSMethod(mm, indexes)),
						at(No) => {}
					),
				_ => {}
			);
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiStatic(ctx, names, from, setter, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiStatic(ctx, names, from, setter, cache));
		}

		return candidates;
	}


	// Categories

	override function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return categories.filter(c -> c.thisType.hasChildType(forType) && c.path.hasChildType(cat))._match(
			at([]) => super.findCategory(ctx, cat, forType, from, cache),
			at(found) => found.concat(super.findCategory(ctx, cat, forType, from, cache))
		);
	}
	
	override function findThisCategory(ctx: Ctx, cat: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return categories.filter(c -> c.type.isNone() && c.path.hasChildType(cat))._match(
			at([]) => super.findThisCategory(ctx, cat, from, cache),
			at(found) => found.concat(super.findThisCategory(ctx, cat, from, cache))
		).concat(parents.flatMap(p -> p.findThisCategory(ctx, cat, from, cache + thisType)).unique());
	}
}