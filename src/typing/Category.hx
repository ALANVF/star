package typing;

import text.Span;
import parsing.ast.Ident;
import errors.Error;
import typing.Traits;

@:build(util.Auto.build())
class Category extends AnyTypeDecl {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var path: Type;
	var type: Null<Type>;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var staticInit: Option<StaticInit> = None;
	var staticDeinit: Option<StaticDeinit> = None;
	var hidden: Null<Option<Type>> = null;
	final friends: Array<Type> = [];

	static function fromAST(lookup: ITypeLookup, ast: parsing.ast.decls.Category) {
		final category = new Category({
			lookup: lookup,
			span: ast.span,
			name: new Ident(ast.path.span(), ast.path.simpleName()),
			path: null, // hack for partial initialization
			type: null  // hack for partial initialization
		});

		var path = (ast.path : TypePath).toType(category);

		category.path = path;
		category.type = ast.type._and(x => category.makeTypePath(x));

		category.thisType = category.type._or(
			(cast lookup : AnyTypeDecl).thisType
		);

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(category, a))) {
			category.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(category.hidden != null): category.errors.push(Type_DuplicateAttribute(category, category.name.name, "hidden", span));
			case IsHidden(None): category.hidden = None;
			case IsHidden(Some(outsideOf)): category.hidden = Some(category.makeTypePath(outsideOf));

			case IsFriend(_) if(category.friends.length != 0): category.errors.push(Type_DuplicateAttribute(category, category.name.name, "friend", span));
			case IsFriend(One(friend)): category.friends.push(category.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) category.friends.push(category.makeTypePath(friend));
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): category.staticMembers.push(Member.fromAST(category, m));
			
			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(category, m).forEach(x -> category.staticMethods.push(x));
			case DMethod(m): category.methods.push(Method.fromAST(category, m));

			case DInit(i): category.inits.push(Init.fromAST(category, i));

			case DOperator(o): Operator.fromAST(category, o).forEach(x -> category.operators.push(x));

			case DDefaultInit(i) if(category.staticInit.isSome()): category.staticInit = Some(StaticInit.fromAST(category, i));
			
			case DDeinit(d) if(category.staticDeinit.isSome()): category.staticDeinit = Some(StaticDeinit.fromAST(category, d));

			default: category.errors.push(Type_UnexpectedDecl(category, decl));
		}

		return category;
	}

	function hasErrors() {
		return errors.length != 0
			|| typevars.allValues().some(g -> g.hasErrors())
			|| staticMembers.some(m -> m.hasErrors())
			|| staticMethods.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	function allErrors() {
		var result = errors;
		
		for(typevar in typevars) result = result.concat(typevar.allErrors());
		for(member in staticMembers) result = result.concat(member.allErrors());
		for(method in staticMethods) result = result.concat(method.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	function declName() {
		return "category";
	}

	function fullName(cache: TypeCache = Nil) {
		return type._andOr(t => t.fullName(cache), lookup._match(
			at(decl is TypeDecl) => decl.fullName(cache),
			at(tvar is TypeVar) => tvar.fullName(cache),
			_ => throw "???"
		)) + "+" + path.fullName(cache);
	}


	override function findType(path: LookupPath, search: Search, from: Null<AnyTypeDecl>, depth = 0, cache: Cache = Nil): Null<Type> {
		if(cache.contains(this)) return null;
		cache += this;

		if(search == Inside) return null;

		return path._match(
			at([[span, "This", args]], when(depth == 0)) => {
				if(args.length == 0) {
					type._andOr(
						t => t,
						lookup.findType(path, Start, from, 0, cache)
					);
				} else {
					// prob shouldn't be attatched to *this* category decl, but eh
					errors.push(Type_NotYetImplemented(span));
					null;
				}
			},
			at([[span, typeName, args], ...rest]) => {//args=args.map(a->a.simplify());
				var finished = true;
				final res: Null<Type> = typevars.find(typeName).map(found -> found.filter(tvar ->
					!cache.contains(tvar.thisType)
					&& (tvar.params.length == 0 || tvar.params.length == args.length)
				))._match(
					at(None | Some([])) => lookup.findType(path, Outside, this, depth, cache)._or(
						thisType.findType(path, Start, this, depth, cache)
					),
					at(Some(_), when(depth != 0)) => lookup.findType(path, Outside, this, depth - 1, cache)._or(
						thisType.findType(path, Start, this, depth - 1, cache)
					),
					at(Some([tvar])) => switch [args, tvar.params] {
						case [[], _]:
							finished = false;
							{t: tvar.thisType.t, span: span}; // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							// error...?
							null;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Type_InvalidTypeApply(span, "Too many arguments"));
								null;
							} else if(args.length < params.length) {
								errors.push(Type_InvalidTypeApply(span, "Not enough arguments"));
								null;
							} else {
								finished = false;
								{t: TApplied(tvar.thisType, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
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
						} else switch found.map(t -> t.thisType) {
							case []:
								errors.push(Type_InvalidTypeApply(span, "No candidate matches the type arguments"));
								null;
							case [tvar]:
								finished = false;
								{t: TApplied(tvar, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Type_InvalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span};
							case tvars:
								finished = false;
								{t: TMulti(tvars), span: span};
						}
					}
				);

				Util._match([rest, res],
					at([_, null]) => lookup.findType(path, Outside, from, depth, cache),
					at([_, _], when(finished)) => res,
					at([Nil3, _]) => res,
					at([_, {t: TConcrete(decl)}]) => decl.findType(rest, Outside, from, 0, cache),
					at([_, type!!]) => {t: TLookup(type, rest, this), span: span}
				);
			},
			_ => throw "bad"
		);
	}

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}


	// Type checking

	function hasParentDecl(decl: TypeDecl) {
		return type._andOr(
			t => t.hasParentDecl(decl),
			lookup._match(
				at(td is TypeDecl) => td.hasParentDecl(decl),
				at(tv is TypeVar) => tv.hasParentDecl(decl),
				_ => false
			)
		);
	}

	function hasChildDecl(decl: TypeDecl) {
		return type._andOr(
			t => t.hasChildDecl(decl),
			lookup._match(
				at(td is TypeDecl) => td.hasChildDecl(decl),
				at(tv is TypeVar) => tv.hasChildDecl(decl),
				_ => false
			)
		);
	}

	function hasParentType(type2: Type) {
		return type._andOr(
			t => t.hasParentType(type2),
			lookup._match(
				at(td is TypeDecl) => td.hasParentType(type2),
				at(tv is TypeVar) => tv.hasParentType(type2),
				_ => false
			)
		);
	}

	function hasChildType(type2: Type) {
		return type._andOr(
			t => t.hasChildType(type2),
			lookup._match(
				at(td is TypeDecl) => td.hasChildType(type2),
				at(tv is TypeVar) => tv.hasChildType(type2),
				_ => false
			)
		);
	}


	function hasStrictChildType(type2: Type): Bool {
		throw "NYI!";
	}


	function hasRefinementDecl(decl: TypeDecl): Bool {
		throw "NYI!";
	}

	function hasRefinementType(type: Type): Bool {
		throw "NYI!";
	}


	// Unification

	function strictUnifyWithType(type: Type): Null<Type> {
		throw "NYI!";
	}


	// Generics

	function acceptsArgs(args: Array<Type>): Bool {
		throw "NYI!";
	}

	function applyArgs(args: Array<Type>): Null<Type> {
		throw "NYI!";
	}


	// Attributes

	function isNative(kind: NativeKind): Bool {
		throw "NYI!";
	}

	function getNative(): Null<NativeKind> {
		throw "NYI!";
	}

	function isFlags(): Bool {
		throw "NYI!";
	}

	function isStrong(): Bool {
		throw "NYI!";
	}

	function isUncounted(): Bool {
		throw "NYI!";
	}


	// Iterating

	function iterElemType() {
		return thisType.iterElemType();
	}

	function iterAssocType() {
		return thisType.iterAssocType();
	}


	// Effects tracking

	function trackEffectsIn(ctx: Ctx): Null<Effects> {
		throw "NYI!";
	}

	function applyArgsTrackEffects(args: Array<Type>, ctx: Ctx): Null<Tuple2<Type, Effects>> {
		throw "NYI!";
	}


	// Privacy

	function canSeeMember(member: Member) {
		return member.decl == this
			|| thisType.canSeeMember(member);
	}

	function canSeeMethod(method: AnyMethod) {
		return method.decl == this
			|| thisType.canSeeMethod(method);
	}

	
	// Members

	function instMembers(from: AnyTypeDecl) {
		return staticMembers.filter(mem -> from.canSeeMember(mem))
			.concat(thisType.instMembers(from));
	}

	function findInstMember(ctx: Ctx, name: String, allowStatic = true, onlyParents = false): Null<MemberKind> {
		if(allowStatic) for(mem in staticMembers) {
			if(mem.name.name == name) {
				return MKMember(mem);
			}
		}

		return thisType.findInstMember(ctx, name, allowStatic, onlyParents);
	}


	// Method lookup

	function hasDefaultInit(): Bool {
		return thisType.hasDefaultInit();
	}

	function findSingleStatic(ctx: Ctx, name: String, from: Type, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

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

		return null;
	}

	function findMultiStatic(ctx: Ctx, names: Array<String>, from: Type, setter = false, cache: TypeCache = Nil): Array<MultiStaticKind> {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

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

			for(init in inits) init._match(
				at(mi is MultiInit) => if(from.canSeeMethod(mi))
					mi.params.matchesNames(names)._match(
						at(Yes) => candidates.push(MSInit(mi)),
						at(Partial(indexes)) => candidates.push(MSInit(mi, indexes)),
						at(No) => {}
					),
				_ => {}
			);
		}

		return candidates;
	}


	function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

		/*for(mem in members) {
			if(mem.matchesGetter(name) && from.canSeeMember(mem)) {
				return SIMember(mem);
			}
		}*/

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

		return null;
	}

	function findMultiInst(ctx: Ctx, names: Array<String>, from: Type, setter = false, cache: TypeCache = Nil): Array<MultiInstKind> {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

		final candidates: Array<MultiInstKind> = [];

		/*for(mem in members) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				return MIMember(mem);
			}
		}*/

		if(setter) {
			throw "todo";
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

		return candidates;
	}


	function findCast(ctx: Ctx, target: Type, from: Type, cache: TypeCache = Nil) {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

		final candidates: Array<CastKind> = [];

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target)) {
					candidates.push(CMethod(cm));
				}
			},
			_ => {}
		);

		return candidates;
	}


	function findUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Null<UnaryOpKind> {
		return null;
	}


	function findBinaryOp(ctx: Ctx, op: BinaryOp, from: Type, cache: TypeCache = Nil): Array<BinaryOpKind> {
		return [];
	}


	// Categories

	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		final found = lookup.findCategory(ctx, cat, forType, from, cache);
		
		if(thisType.hasChildType(forType) && path.hasChildType(cat)) {
			return found.concat([this]);
		} else {
			return found;
		}
	}
}