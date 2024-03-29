package typing;

private final EMPTY_ARRAY: Array<Member> = [];

@:structInit
class TaggedKind extends Kind {
	final taggedCases: Array<TaggedCase> = [];
	var defaultInit: Option<DefaultInit> = None;

	static function fromAST(lookup: ITypeLookup, ast: parsing.ast.decls.Kind) {
		final kind: TaggedKind = {
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		};

		kind.initThisType();

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(kind, a))) {
			kind.typevars.add(typevar.name.name, typevar);
		}

		ast.params._and(params => {
			kind.params = params.of.map(param -> kind.makeTypePath(param));
		});

		ast.repr._and(repr => {
			kind.errors.push(Type_NoTaggedKindRepr(
				kind,
				repr.span()
			));
		});

		ast.parents._and(parents => {
			for(parent in parents.parents) {
				kind.parents.push(kind.makeTypePath(parent));
			}
		});

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(kind.hidden != null): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "hidden", span));
			case IsHidden(None): kind.hidden = None;
			case IsHidden(Some(outsideOf)): kind.hidden = Some(kind.makeTypePath(outsideOf));

			case IsFriend(_) if(kind.friends.length != 0): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "friend", span));
			case IsFriend(One(friend)): kind.friends.push(kind.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) kind.friends.push(kind.makeTypePath(friend));

			case IsSealed(_) if(kind.sealed.isSome()): kind.errors.push(Type_DuplicateAttribute(kind, ast.name.name, "sealed", span));
			case IsSealed(None): kind.sealed = Some(None);
			case IsSealed(Some(outsideOf)): kind.sealed = Some(Some(kind.makeTypePath(outsideOf)));

			case IsFlags: kind._isFlags = true;

			case IsStrong: kind._isStrong = true;

			case IsUncounted: kind._isUncounted = true;
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): kind.staticMembers.push(Member.fromAST(kind, m));
			case DMember(m): kind.members.push(Member.fromAST(kind, m));

			case DCase(c = {kind: Tagged(_)}): kind.taggedCases.push(TaggedCase.fromAST(kind, c));

			case DModule(m): kind.addTypeDecl(Module.fromAST(kind, m));

			case DClass(c): kind.addTypeDecl(Class.fromAST(kind, c));

			case DProtocol(p): kind.addTypeDecl(Protocol.fromAST(kind, p));
			
			case DKind(k): kind.addTypeDecl(Kind.fromAST(kind, k));

			case DAlias(a): kind.addTypeDecl(Alias.fromAST(kind, a));

			case DCategory(c): kind.categories.push(Category.fromAST(kind, c));

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(kind, m).forEach(x -> kind.staticMethods.push(x));
			case DMethod(m): kind.methods.push(Method.fromAST(kind, m));

			case DOperator(o): Operator.fromAST(kind, o).forEach(x -> kind.operators.push(x));

			case DDefaultInit(i) if(kind.staticInit.isSome()): kind.staticInit = Some(StaticInit.fromAST(kind, i));
			case DDefaultInit(i): kind.defaultInit = Some(DefaultInit.fromAST(kind, i));
			
			case DDeinit(d) if(kind.staticDeinit.isSome()): kind.staticDeinit = Some(StaticDeinit.fromAST(kind, d));
			case DDeinit(d): kind.deinit = Some(Deinit.fromAST(kind, d));
			
			default: kind.errors.push(Type_UnexpectedDecl(kind, decl));
		}

		return kind;
	}

	override function hasErrors() {
		return super.hasErrors()
			|| taggedCases.some(c -> c.hasErrors())
			|| members.some(m -> m.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(taggedCase in taggedCases) result = result.concat(taggedCase.allErrors());
		for(member in members) result = result.concat(member.allErrors());
		
		return result;
	}


	// Cases

	override function allTaggedCases(): Array<TaggedCase> {
		var res = [];

		for(parent in parents) {
			res = res.concat(parent.allTaggedCases());
		}

		for(ref in refinees) {
			res = res.concat(ref.allTaggedCases());
		}

		res = res.concat(taggedCases);

		return res;
	}


	// Method lookup

	override function hasDefaultInit(): Bool {
		return defaultInit.isSome() || super.hasDefaultInit();
	}

	override function findSingleStatic(ctx: Ctx, name: String, from: Type, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		if(!getter) for(tcase in taggedCases) {
			tcase._match(
				at(scase is SingleTaggedCase) => if(scase.name.name == name) return SSTaggedCase(scase),
				_ => {}
			);

			tcase.assoc._match(
				at(Single(_, _, sname), when(sname == name)) => return SSTaggedCaseAlias(tcase),
				_ => {}
			);
		}

		return super.findSingleStatic(ctx, name, from, getter, cache);
	}

	
	override function findMultiStatic(ctx: Ctx, names: Array<String>, from: Type, setter = false, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];
		
		var candidates: Array<MultiStaticKind> = [];
		if(!setter) for(tcase in taggedCases) {
			tcase._match(
				at(mcase is MultiTaggedCase) => {
					if(mcase.params.every2Strict(names, (l, n) -> l.label.name == n)) {
						candidates = [MSTaggedCase(/*[] HAXE DUMB DON'T DO THIS */ EMPTY_ARRAY, mcase, EMPTY_ARRAY)]; break;
					} else {
						// BAD
						if(/*!names.contains("_") &&*/ names.isUnique()) {
							final mems = instMembers(this);
							final found1 = [];
							final found2 = [];
							var bad = false;

							var begin = 0;
							while(begin < names.length) {
								final name = names[begin];

								mems.find(mem -> mem.name.name == name)._andOr(mem => {
									if(from.canSeeMember(mem)) {
										found1.push(mem);
									} else {
										bad = true;
										break;
									}
								}, {
									break;
								});

								begin++;
							}

							var end = names.length - 1;
							while(begin < end) {
								final name = names[end];

								mems.find(mem -> mem.name.name == name)._andOr(mem => {
									if(from.canSeeMember(mem)) {
										found2.unshift(mem);
									} else {
										bad = true;
										break;
									}
								}, {
									break;
								});

								end--;
							}

							if(!bad) {
								final subNames = names.slice(begin, end + 1);

								mcase.params.matchesNames(subNames)._match(
									at(Yes) => {
										candidates = [MSTaggedCase(found1, mcase, found2)]; break;
									},
									at(Partial(indexes)) => {
										candidates = [MSTaggedCase(found1, mcase, found2, indexes)]; break;
									},
									at(No) => {}
								);
							}
						}
					}
				},
				_ => {}
			);

			tcase.assoc._match(
				at(Multi(_, labels), when(
					labels.every2Strict(names, (l, n) -> switch l {
						case Named(_, name, _) | Punned(_, name): name == n;
						case Anon(_): n == "_";
					})
				)) => {
					candidates = [MSTaggedCaseAlias(tcase)]; break;
				},
				_ => {}
			);
		}
		
		return candidates.concat(super.findMultiStatic(ctx, names, from, setter, cache));
	}


	/*
	override function findCast(ctx: Ctx, target: Type, from: Type, cache: TypeCache = Nil) {
		if(cache.contains(thisType)) return [];
		
		final candidates = super.findCast(ctx, target, from, cache);

		return if(target.hasParentDecl(this)) {
			candidates.concat([CUpcast(target)]);
		} else {
			candidates;
		}
	}
	*/


	override function findBinaryOp(ctx: Ctx, op: BinaryOp, from: Type, cache: TypeCache = Nil) {
		final res = super.findBinaryOp(ctx, op, from, cache);

		if(_isFlags) {
			return res.concat(Pass2.STD_MultiKind.findBinaryOp(ctx, op, from, cache));
		} else {
			return res;
		}
	}
}