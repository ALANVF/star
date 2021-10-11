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


	override function isNative(kind: NativeKind) {
		return parents.some(p -> p.isNative(kind));
	}


	inline function addTypeDecl(decl: TypeDecl) {
		decls.add(decl.name.name, decl);
	}

	override function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		//if(cache.contains(this)) return None;
		//cache = cache.prepend(thisType);

		if(from == null) from = this;

		return path._match(
			at([[span, "This", args]], when(search != Inside && depth == 0)) => {
				if(args.length == 0) {
					Some({t: TThis(this), span: span});
				} else {
					// errors prob shouldn't be attatched to *this* type decl, but eh
					if(params.length == 0) {
						errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
						None;
					} else if(args.length > params.length) {
						errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
						None;
					} else if(args.length < params.length) {
						errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
						None;
					} else {
						Some({t: TApplied({t: TThis(this), span: span}, args), span: span});
					}
				}
			},
			at([[span, typeName, args], ...rest]) => {
				var finished = true;
				final res: Option<Type> = (if(search == Inside || typevars.size == 0) {
					cast decls.find(typeName);
				} else {
					final ds: Option<Array<IFullTypeDecl>> = cast decls.find(typeName);
					final tvs: Option<Array<IFullTypeDecl>> = cast typevars.find(typeName);

					tvs.doOrElse(
						_tvs => ds.doOrElse(
							_ds => Some(_ds.concat(_tvs)),
							tvs
						),
						ds
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
					at(None | Some([])) => if(search == Inside) None else lookup.findType(path, Outside, from, depth, cache),
					at(Some(_), when(depth != 0)) => {
						if(search == Inside) {
							None;
						} else {
							lookup.findType(path, Outside, from, depth - 1, cache);
						}
					},
					at(Some([type])) => switch [args, type.params] {
						case [[], _]:
							finished = false;
							Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							if(search == Inside) {
								errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
								None;
							} else {
								// error...?
								lookup.findType(path, Outside, from, depth, cache);
							}
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								finished = false;
								Some({t: TApplied(type.thisType, args), span: span});
							}
					},
					at(Some(found)) => {
						if(args.length == 0) {
							finished = false;
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(t -> t.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]:
								finished = false;
								Some({t: TApplied(type, args), span: span});
							case types:
								finished = false;
								Some({t: TMulti(types), span: span});
						}
					}
				);

				switch [rest, res] {
					case [_, None]: /*if(search == Inside &&
						lookup._match(
							at(f is File) => !f.unit.exists(u -> u.primary.contains(f)),
							_ => true
						)
					)*/lookup.findType(path, Outside, from, depth, cache);
					case [_, _] if(finished): res;
					case [Nil3, _]: res;
					case [_, Some({t: TConcrete(decl)})]: decl.findType(rest, Inside, from, 0, cache);
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => throw "bad"
		);
	}

	/*override function findTypeOld(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(absolute) {
			if(cache.contains(this)) {
				return None;
			} else {
				cache = cache.prepend(this);
			}
		}
		
		return path._match(
			at([[span, "This", []]], when(absolute)) => Some({t: TThis(this), span: span}),
			at([[span, "This", args]], when(absolute)) => {
				// errors prob shouldn't be attatched to *this* type decl, but eh
				if(params.length == 0) {
					errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
					None;
				} else if(args.length > params.length) {
					errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
					None;
				} else if(args.length < params.length) {
					errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
					None;
				} else {
					Some({t: TApplied({t: TThis(this), span: span}, args), span: span});
				}
			},
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch (absolute ? {
					final ts: Option<Array<IFullTypeDecl>> = cast decls.find(typeName);
					final tvs: Option<Array<IFullTypeDecl>> = cast typevars.find(typeName);
					tvs.doOrElse(
						_tvs => ts.doOrElse(
							_ts => Some(_ts.concat(_tvs)),
							tvs
						),
						ts
					);
				} : cast decls.find(typeName)) {
					case None: return if(absolute) lookup.findTypeOld(path, true, cache) else None;
					case Some([type]) if(cache.contains(type)): return lookup.findTypeOld(path, true, cache.prepend(this.thisType));
					case Some([type]): switch [args, type.params] {
						case [[], _]: Some(type.thisType); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							errors.push(Errors.invalidTypeApply(span, "Attempt to apply arguments to a non-parametric type"));
							None;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								Some({t: TApplied(type.thisType, args), span: span});
							}
					}
					case Some(found):
						if(args.length == 0) {
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.filter(t -> t.params.length == args.length).map(g -> g.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No matching candidates were found"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					// is this really needed?
					case [_, None]: if(absolute && rest == Nil3) lookup.findTypeOld(path, true, cache) else lookup._match(
						at(file is File) => file.dir._match(
							at(unit is Unit) => if(unit.primary.contains(file)) {
								unit.findTypeOld(rest, false, cache.prepend(file));
							} else {
								throw "???";
							},
							_ => None
						),
						_ => None
					);
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => if(absolute) lookup.findTypeOld(path, true, cache) else None
		);
	}*/

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


	override function hasParentDecl(decl: TypeDecl) {
		return this == decl
			|| parents.some(p -> p.hasParentDecl(decl))
			|| super.hasParentDecl(decl);
	}


	override function hasChildDecl(decl: TypeDecl) {
		return this == decl
			|| refinements.some(r -> r.hasChildDecl(decl)
				|| (r.params.length != 0 && r.params.length == decl.params.length && {
					r.params.every2(decl.params, (p1, p2) -> p1.hasChildType(p2));
				}))
			|| super.hasChildDecl(decl);
	}


	override function canSeeMember(member: Member) {
		return super.canSeeMember(member)
			|| parents.some(p -> p.canSeeMember(member));
	}
	
	override function canSeeMethod(method: AnyMethod) {
		return super.canSeeMethod(method)
			|| parents.some(p -> p.canSeeMethod(method));
	}


	function defaultSingleStatic(name: String, from: ITypeDecl, getter = false): Null<SingleStaticKind> {
		return null;
	}

	override function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
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
			_ => {}
		);
		
		for(parent in parents) {
			parent.findSingleStatic(name, from, getter, cache)._match(
				at(ss!) => return ss,
				_ => {}
			);
		}

		return defaultSingleStatic(name, from, getter);
	}


	override function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
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
				at(mm is MultiStaticMethod) => if(from.canSeeMethod(mm)) {
					if(mm.params.every2Strict(names, (l, n) -> l.label.name == n)) {
						candidates.push(MSMethod(mm));
					} else if(names.length < mm.params.length) {
						var n = 0;
						var p = 0;
						var matchedOnce = false;
						while(n < names.length && p < mm.params.length) {
							mm.params[p]._match(
								at({label: {name: label}, value: _}, when(label == names[n])) => {
									n++;
									p++;
									if(!matchedOnce) matchedOnce = true;
								},
								
								at({label: {name: _}, value: _!}) => {
									p++;
								},

								_ => {
									matchedOnce = false;
									break;
								}
							);
						}

						if(matchedOnce) {
							candidates.push(MSMethod(mm, true));
						}
					}
				},
				_ => {}
			);
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiStatic(names, from, setter, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiStatic(names, from, setter, cache));
		}

		return candidates;
	}


	override function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return categories.filter(c -> c.thisType.hasChildType(forType) && c.path.hasChildType(cat))._match(
			at([]) => super.findCategory(cat, forType, from, cache),
			at(found) => found.concat(super.findCategory(cat, forType, from, cache))
		);
	}
	
	override function findThisCategory(cat: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return categories.filter(c -> c.type.isNone() && c.path.hasChildType(cat))._match(
			at([]) => super.findThisCategory(cat, from, cache),
			at(found) => found.concat(super.findThisCategory(cat, from, cache))
		);
	}
}