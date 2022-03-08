package typing;

import typing.Traits;

class StrongAlias extends Alias {
	var type: Type;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final members: Array<Member> = [];
	final methods: Array<Method> = [];
	final operators: Array<Operator> = [];
	var noInherit: Bool = false;

	static function fromAST(lookup, ast: parsing.ast.decls.Alias) {
		final alias = new StrongAlias({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: [],
			type: null // Hack for partial initialization
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(alias, a))) {
			alias.typevars.add(typevar.name.name, typevar);
		}

		final body = switch ast.kind {
			case Strong(type, body):
				alias.type = alias.makeTypePath(type); // Fix
				body;
			default: throw "Error!";
		};

		ast.params._and(params => {
			alias.params = params.of.map(param -> alias.makeTypePath(param));
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(alias.hidden.isSome()): alias.errors.push(Type_DuplicateAttribute(alias, ast.name.name, "hidden", span));
			case IsHidden(None): alias.hidden = Some(None);
			case IsHidden(Some(outsideOf)): alias.hidden = Some(Some(alias.makeTypePath(outsideOf)));

			case IsFriend(_) if(alias.friends.length != 0): alias.errors.push(Type_DuplicateAttribute(alias, ast.name.name, "friend", span));
			case IsFriend(One(friend)): alias.friends.push(alias.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) alias.friends.push(alias.makeTypePath(friend));
			
			case IsNoinherit: alias.noInherit = true;
		}

		if(body.isSome()) {
			for(decl in body.value().of) switch decl {
				case DMember(m) if(m.attrs.exists(IsStatic)): alias.staticMembers.push(Member.fromAST(alias, m));
				case DMember(m): alias.members.push(Member.fromAST(alias, m));

				case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(alias, m).forEach(x -> alias.staticMethods.push(x));
				case DMethod(m): alias.methods.push(Method.fromAST(alias, m));
	
				case DOperator(o): Operator.fromAST(alias, o).forEach(x -> alias.operators.push(x));
	
				default: alias.errors.push(Type_UnexpectedDecl(alias, decl));
			}
		}

		return alias;
	}
	
	override function hasErrors() {
		return super.hasErrors()
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| members.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();

		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(member in members) result = result.concat(member.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}
	
	override function declName() {
		return "strong alias";
	}

	
	// Type checking

	override function hasParentDecl(decl: TypeDecl) {
		return super.hasParentDecl(decl)
			|| (!noInherit && type.hasParentDecl(decl));
	}

	override function hasChildDecl(decl: TypeDecl) {
		return super.hasChildDecl(decl)
			|| (!noInherit && type.hasChildDecl(decl));
	}

	
	override function hasParentType(type2: Type) {
		return super.hasParentType(type2)
			|| (!noInherit && type.hasParentType(type2));
	}

	override function hasChildType(type2: Type) {
		return super.hasChildType(type2)
			|| (!noInherit && type.hasChildType(type2));
	}


	// Attributes

	override function isNative(kind: NativeKind) {
		return !noInherit && type.isNative(kind);
	}

	override function isFlags() {
		return !noInherit && type.isFlags();
	}
	
	override function isStrong() {
		return !noInherit && type.isStrong();
	}

	override function isUncounted() {
		return !noInherit && type.isUncounted();
	}


	// Iterating

	override function iterElemType() {
		return type.iterElemType()._and(ty => ty.getFrom(thisType));
	}

	override function iterAssocType() {
		return type.iterAssocType()._match(
			at(null) => null,
			at({_1: k, _2: v}) => new Tuple2(k.getFrom(thisType), v.getFrom(thisType))
		);
	}


	// Privacy

	override function canSeeMember(member: Member) {
		return super.canSeeMember(member)
			|| type.canSeeMember(member);
	}
	
	override function canSeeMethod(method: AnyMethod) {
		return super.canSeeMethod(method)
			|| type.canSeeMethod(method);
	}


	// Members

	override function instMembers(from: AnyTypeDecl) {
		return staticMembers.concat(members).filter(mem -> from.canSeeMember(mem))
			.concat(noInherit ? [] : type.instMembers(from));
	}

	override function findInstMember(ctx: Ctx, name: String, allowStatic = true, onlyParents = false): Null<MemberKind> {
		if(!onlyParents) {
			if(allowStatic) for(mem in staticMembers) {
				if(mem.name.name == name) {
					return MKMember(mem);
				}
			}

			for(mem in members) {
				if(mem.name.name == name) {
					return MKMember(mem);
				}
			}
		}

		return noInherit ? null : type.findInstMember(ctx, name, allowStatic)._and(res => {
			if(params.length > 0) {
				MKFromParent(type, res);
			} else {
				res;
			}
		});
	}


	// Method lookup

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
		
		if(!noInherit) type.findSingleStatic(ctx, name, from, getter, cache + thisType)._match(
			at(ss!) => return SSFromParent(type, ss),
			_ => {}
		);

		return null;
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

		if(!noInherit) {
			candidates.pushAll(type.findMultiStatic(ctx, names, from, setter, cache + thisType));
		}

		return candidates;
	}


	override function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
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

		for(refinee in refinees) {
			refinee.findSingleInst(ctx, name, from, getter, cache)._match(
				at(si!) => return si,
				_ => {}
			);
		}
		
		return noInherit ? null : type.findSingleInst(ctx, name, from, getter, cache + thisType)._and(k => SIFromParent(type, k));
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

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiInst(ctx, names, from, setter, cache));
		}

		if(!noInherit) {
			candidates.pushAll(type.findMultiInst(ctx, names, from, setter, cache + thisType));
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

		for(refinee in refinees) {
			candidates.pushAll(refinee.findCast(ctx, target, from, cache));
		}

		if(!noInherit) {
			if(target.strictUnifyWithType(type) != null) { // TODO: check for cast overloads?
				candidates.push(CUpcast(target));
			}

			candidates.pushAll(type.findCast(ctx, target, from, cache + thisType));
		}

		return candidates;
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

		for(refinee in refinees) {
			refinee.findUnaryOp(ctx, op, from, cache)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		if(!noInherit) {
			type.findUnaryOp(ctx, op, from, cache + thisType)._match(
				at(uo!) => return uo,
				_ => {}
			);
		}

		return null;
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

		for(refinee in refinees) {
			candidates.pushAll(refinee.findBinaryOp(ctx, op, from, cache));
		}

		if(!noInherit) {
			candidates.pushAll(type.findBinaryOp(ctx, op, from, cache + thisType));
		}

		return candidates;
	}


	override function findThisCategory(ctx: Ctx, cat: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		var res = lookup.findCategory(ctx, cat, thisType, from, cache + thisType);

		if(res.length == 0 && !noInherit) {
			return lookup.findCategory(ctx, cat, type, from, cache + thisType + type);
		} else {
			return res;
		}
	}
}