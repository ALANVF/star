package typing;

import typing.Traits;

abstract class ClassLike extends Namespace {
	final members: Array<Member> = [];
	final methods: Array<Method> = [];


	override function instMembers(from: TypeDecl) {
		return members.filter(mem -> from.canSeeMember(mem))
			.concat(parents.flatMap(p -> p.instMembers(from)))
			.concat(super.instMembers(from));
	}


	function defaultSingleInst(name: String, from: ITypeDecl, getter = false): Null<SingleInstKind> {
		return null;
	}

	override function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleInstKind> {
		if(cache.contains(thisType)) return null;

		for(mem in members) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SIMember(mem);
			}
		}

		for(mth in methods) mth._match(
			at(sm is SingleMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SIMethod(sm);
				}
			},
			_ => {}
		);

		if(params.length != 0) lookup.findType(List3.of([this.name.span, this.name.name, params]), Inside, this, 0, cast cache)._match(
			at(Some({t: TApplied({t: TConcrete(decl)}, _) | TConcrete(decl)}), when(decl != this)) => {
				if(params.every2(decl.params, (p1, p2) -> p1.hasChildType(p2))) {
					decl.findSingleInst(name, from, getter, cache.prepend(thisType))._match(
						at(si!) => return si,
						_ => {}
					);
				}
			},
			at(Some({t: TMulti(types)})) => {
				for(type in types) switch type.t {
					case TApplied({t: TConcrete(decl)}, _) | TConcrete(decl): if(decl != this) {
						if(params.every2(decl.params, (p1, p2) -> p1.hasChildType(p2))) {
							decl.findSingleInst(name, from, getter, cache.prepend(thisType))._match(
								at(si!) => return si,
								_ => {}
							);
						}
					}
					default: throw "???";
				}
			},
			_ => {}
		);
		
		for(parent in parents) {
			parent.findSingleInst(name, from, getter, cache)._match(
				at(si!) => return si,
				_ => {}
			);
		}
		
		/*if(params.length != 0) {
			lookup.findTypeOld(List3.of([this.name.span, this.name.name, params]), false, cast cache.prepend(thisType))._match(
				at(Some({t: TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)})) => {
					decl.findSingleInst(name, from, getter, cache.prepend(thisType))._match(
						at(kind!) => return kind,
						_ => {}
					);
				},
				at(Some({t: TMulti(types)})) => for(ty in types) ty.t._match(
					at(TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)) => {
						decl.findSingleInst(name, from, getter, cache.prepend(thisType))._match(
							at(kind!) => return kind,
							_ => {}
						);
					},
					_ => {}
				),
				_ => {}
			);
		}*/

		return defaultSingleInst(name, from, getter);
	}


	override function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates: Array<MultiInstKind> = [];

		names._match(at([name]) => for(mem in members) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				candidates.push(MIMember(mem));
			}
		}, _ => {});

		if(setter) {
			switch names {
				case [name]: for(mth in methods) mth._match(
					at(mm is MultiMethod) => if(mm.isSetter) mm.params._match(
						at([{label: {name: l}}], when(l == name && from.canSeeMethod(mm))) => {
							candidates.push(MIMethod(mm));
						},
						_ => {}
					),
					_ => {}
				);

				default: for(mth in methods) mth._match(
					at(mm is MultiMethod) => if(mm.isSetter) {
						if(mm.params.every2Strict(names, (l, n) -> (n == "=" && mm.isSetter) || l.label.name == n) && from.canSeeMethod(mm)) {
							candidates.push(MIMethod(mm));
						}
					},
					_ => {}
				);
			}
		} else {
			for(mth in methods) mth._match(
				at(mm is MultiMethod) => {
					if(mm.params.every2Strict(names, (l, n) -> (n == "=" && mm.isSetter) || l.label.name == n) && from.canSeeMethod(mm)) {
						candidates.push(MIMethod(mm));
					}
				},
				_ => {}
			);
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiInst(names, from, setter, cache));
		}

		return candidates;
	}


	override function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil) {
		if(cache.contains(thisType)) return [];

		final candidates = [];

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target)) {
					candidates.push(cm);
				}
			},
			_ => {}
		);

		for(parent in parents) {
			candidates.pushAll(parent.findCast(target, from, cache));
		}

		return candidates;
	}
}