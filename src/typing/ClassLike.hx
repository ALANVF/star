package typing;

import typing.Traits;

abstract class ClassLike extends Namespace {
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];


	// Members

	override function instMembers(from: AnyTypeDecl) {
		return members.filter(mem -> from.canSeeMember(mem))
			.concat(parents.flatMap(p -> p.instMembers(from).map(
				mem -> new Member({
					errors: mem.errors,
					decl: mem.decl,
					span: mem.span,
					name: mem.name,
					type: mem.type._and(t => t.getFrom(p)),
					isStatic: mem.isStatic,
					hidden: mem.hidden,
					isReadonly: mem.isReadonly,
					getter: mem.getter,
					setter: mem.setter,
					noInherit: mem.noInherit,
					value: mem.value
				})
			)))
			.concat(super.instMembers(from));
	}

	override function findInstMember(ctx: Ctx, name: String, allowStatic = true, onlyParents = false): Null<MemberKind> {
		if(!onlyParents) {
			for(mem in members) {
				if(mem.name.name == name) {
					return MKMember(mem);
				}
			}
		}

		return super.findInstMember(ctx, name, allowStatic, onlyParents);
	}


	// Method lookup

	function defaultSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false): Null<SingleInstKind> {
		return null;
	}

	override function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
		if(cache.contains(thisType)) return null;

		for(mth in methods) mth._match(
			at(sm is SingleMethod) => {
				if(sm.name.name == name && (!getter || sm.isGetter) && from.canSeeMethod(sm)) {
					return SIMethod(sm);
				}
			},
			at(mm is MultiMethod) => if(!getter) {
				if(mm.params[0].label.name == name && mm.params.every(p -> p.value != null)) {
					return SIMultiMethod(mm);
				}
			},
			_ => {}
		);

		for(mem in members) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SIMember(mem);
			}
		}
		
		for(parent in parents) {
			parent.findSingleInst(ctx, name, from, getter, cache)._match(
				at(si!) => return SIFromParent(parent, si),
				_ => {}
			);
		}

		for(refinee in refinees) {
			refinee.findSingleInst(ctx, name, from, getter, cache)._match(
				at(si!) => return si,
				_ => {}
			);
		}
		
		return cache.list._match(
			at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)} is Type, ..._]) => defaultSingleInst(ctx, name, from, getter),
			_ => null
		);
	}


	override function findMultiInst(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil) {
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
					at(mm is MultiMethod) => if(mm.isSetter && from.canSeeMethod(mm))
						mm.params.matchesNames(names, true)._match(
							at(Yes) => candidates.push(MIMethod(mm)),
							at(Partial(indexes)) => candidates.push(MIMethod(mm, indexes)),
							at(No) => {}
						),
					_ => {}
				);
			}
		} else {
			for(mth in methods) mth._match(
				at(mm is MultiMethod) => if(from.canSeeMethod(mm))
					mm.params.matchesNames(names, mm.isSetter)._match(
						at(Yes) => candidates.push(MIMethod(mm)),
						at(Partial(indexes)) => candidates.push(MIMethod(mm, indexes)),
						at(No) => {}
					),
				_ => {}
			);
		}

		for(parent in parents) {
			for(mi in parent.findMultiInst(ctx, names, from, setter, cache)) {
				candidates.push(MIFromParent(parent, mi));
			}
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiInst(ctx, names, from, setter, cache));
		}

		return candidates;
	}


	override function findCast(ctx: Ctx, target: Type, from: AnyTypeDecl, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];

		final candidates: Array<CastKind> = [];

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target) && from.canSeeMethod(cm)) {
					candidates.push(CMethod(cm));
				}
			},
			_ => {}
		);

		for(parent in parents) {
			candidates.pushAll(parent.findCast(ctx, target, from, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findCast(ctx, target, from, cache));
		}

		return candidates.concat(super.findCast(ctx, target, from, cache));
	}

	
	function defaultUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl): Null<UnaryOpKind> {
		return null;
	}

	override function findUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Null<UnaryOpKind> {
		if(cache.contains(thisType)) return null;

		for(oper in operators) oper._match(
			at(unary is UnaryOperator) => {
				if(unary.op == op && from.canSeeMethod(unary)) {
					return UOMethod(unary);
				}
			},
			_ => {}
		);

		for(parent in parents) {
			parent.findUnaryOp(ctx, op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		for(refinee in refinees) {
			refinee.findUnaryOp(ctx, op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		return cache.list._match(
			at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)}, ..._]) => defaultUnaryOp(ctx, op, from),
			_ => null
		);
	}


	function defaultBinaryOp(ctx: Ctx, op: BinaryOp, from: AnyTypeDecl): Array<BinaryOpKind> {
		return [];
	}

	override function findBinaryOp(ctx: Ctx, op: BinaryOp, from: AnyTypeDecl, cache: TypeCache = Nil) {
		final candidates: Array<BinaryOpKind> = [];

		for(oper in operators) oper._match(
			at(binary is BinaryOperator) => {
				if(binary.op == op && from.canSeeMethod(binary)) {
					candidates.push(BOMethod(binary));
				}
			},
			_ => {}
		);

		for(parent in parents) {
			candidates.pushAll(parent.findBinaryOp(ctx, op, from, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findBinaryOp(ctx, op, from, cache));
		}

		return candidates._match(
			at([]) => cache.list._match(
				at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)}, ..._]) => defaultBinaryOp(ctx, op, from),
				_ => candidates
			),
			_ => candidates
		);
	}
}