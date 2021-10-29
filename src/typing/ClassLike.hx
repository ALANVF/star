package typing;

import typing.Traits;

abstract class ClassLike extends Namespace {
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];


	override function instMembers(from: ITypeDecl) {
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
			at(mm is MultiMethod) => if(!getter) {
				if(mm.params[0].label.name == name && mm.params.every(p -> p.value != null)) {
					return SIMultiMethod(mm);
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

		for(refinee in refinees) {
			refinee.findSingleInst(name, from, getter, cache)._match(
				at(si!) => return si,
				_ => {}
			);
		}
		
		return cache._match(
			at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)} is Type, ..._]) => defaultSingleInst(name, from, getter),
			_ => null
		);
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
					at(mm is MultiMethod) => if(mm.isSetter && from.canSeeMethod(mm))
						mm.params.matchesNames(names, true)._match(
							at(Yes) => candidates.push(MIMethod(mm)),
							at(Partial) => candidates.push(MIMethod(mm, true)),
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
						at(Partial) => candidates.push(MIMethod(mm, true)),
						at(No) => {}
					),
				_ => {}
			);
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiInst(names, from, setter, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiInst(names, from, setter, cache));
		}

		return candidates;
	}


	override function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil) {
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
			candidates.pushAll(parent.findCast(target, from, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findCast(target, from, cache));
		}

		return candidates;
	}

	
	function defaultUnaryOp(op: UnaryOp, from: ITypeDecl): Null<UnaryOpKind> {
		return null;
	}

	override function findUnaryOp(op: UnaryOp, from: ITypeDecl, cache: List<Type> = Nil): Null<UnaryOpKind> {
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
			parent.findUnaryOp(op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		for(refinee in refinees) {
			refinee.findUnaryOp(op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		return cache._match(
			at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)}, ..._]) => defaultUnaryOp(op, from),
			_ => null
		);
	}


	function defaultBinaryOp(op: BinaryOp, from: ITypeDecl): Array<BinaryOpKind> {
		return [];
	}

	override function findBinaryOp(op: BinaryOp, from: ITypeDecl, cache: List<Type> = Nil) {
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
			candidates.pushAll(parent.findBinaryOp(op, from, cache));
		}

		for(refinee in refinees) {
			candidates.pushAll(refinee.findBinaryOp(op, from, cache));
		}

		return candidates._match(
			at([]) => cache._match(
				at([] | [{t: TConcrete(_ is DirectAlias | _ is StrongAlias)}, ..._]) => defaultBinaryOp(op, from),
				_ => candidates
			),
			_ => candidates
		);
	}
}