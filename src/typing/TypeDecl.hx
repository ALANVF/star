package typing;

import reporting.Diagnostic;
import text.Span;
import typing.Traits;

@:build(util.Auto.build({keepInit: true}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl extends AnyFullTypeDecl {
	@ignore final typevars = new MultiMap<String, TypeVar>();
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	@ignore final refinements = new Array<TypeDecl>();
	@ignore final refinees = new Array<TypeDecl>();

	function new() {
		thisType = new Type(TConcrete(this), null);
	}

	function fullName(cache: TypeCache = Nil) {
		cache += thisType;
		return switch params {
			case []: Type.getFullPath(this).value();
			default: Type.getFullPath(this).value() + "[" + params.joinMap(", ", p ->
				if(cache.contains(p)) {
					"...";
				} else {
					p.fullName(cache);
				}
			) + "]";
		}
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
				final res: Null<Type> = (search == Inside ? None : typevars.find(typeName).map(found -> found.filter(tvar ->
					!cache.contains(tvar.thisType)
					&& (tvar.params.length == 0 || tvar.params.length == args.length)
				)))._match(
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
								{t: TApplied({t: type.thisType.t, span: span}, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(type!) => type,
										_ => {
											errors.push(Errors.invalidTypeLookup(lookup.span(), 'Unknown type `${arg.simpleName()}`'));
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
							{t: TMulti(found.map(t -> new Type(t.thisType.t, span))), span: span};
						} else switch found.filter(t -> t.params.length == args.length).map(t -> new Type(t.thisType.t, span)) {
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
	
	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}

	function hasErrors() {
		return errors.length != 0
			|| typevars.allValues().some(g -> g.hasErrors());
	}

	function allErrors() {
		var result = errors;

		for(typevar in typevars) result = result.concat(typevar.allErrors());

		return result;
	}


	function buildRefinements() {
		if(params.length != 0) {
			lookup.findType(List3.of([this.name.span, this.name.name, this.params]), Inside, this, 0, List.of(thisType))._match(
				at({t: TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)}) => {
					if(this != decl
					&& name.name == decl.name.name
					&& lookup == decl.lookup
					&& this.applyArgs(decl.params)!=null
					&& !decl.refinements.contains(this)) {
						this.refinements.push(decl);
						decl.refinees.push(this);
					}
				},
				at({t: TApplied({t: TMulti(types)}, _)}) => for(ty in types) ty.t._match(
					at(TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)) => {
						if(this != decl
						&& name.name == decl.name.name
						&& lookup == decl.lookup
						&& this.applyArgs(decl.params)!=null
						&& !decl.refinements.contains(this)) {
							this.refinements.push(decl);
							decl.refinees.push(this);
						}
					},
					_ => {}
				),
				_ => {}
			);
		}
	}


	// Type checking

	function hasParentDecl(decl: TypeDecl) {
		return this == decl
			|| (decl == Pass2.STD_Value && !this.isNative(NVoid))
			|| refinees.some(r -> r == decl || r.hasParentDecl(decl));
	}

	function hasChildDecl(decl: TypeDecl) {
		return this == decl
			//|| (this == Pass2.STD_Value && decl != Pass2.STD_Void)
			|| refinements.some(r -> r == decl || r.hasChildDecl(decl))
			|| refinees.some(r -> r == decl);
			//|| decl.hasParentDecl(this);
	}


	function hasParentType(type: Type) {
		return thisType == type
			//|| (!this.isVoid() && type == Pass2.STD_Value.thisType)
			|| refinees.some(r -> r.thisType == type || r.hasParentType(type))
			|| type.hasChildDecl(this);
	}

	function hasChildType(type: Type) {
		return thisType == type
			|| (this == Pass2.STD_Value && !type.isNative(NVoid))
			|| refinements.some(r -> r.thisType == type || r.hasChildType(type))
			|| type.hasParentDecl(this);
	}


	function hasStrictChildType(type: Type): Bool {
		throw "NYI!";
	}

	
	function hasRefinementDecl(decl: TypeDecl): Bool {
		return refinements.contains(decl) || refinements.some(ref -> ref.hasRefinementDecl(decl));
	}

	function hasRefinementType(type: Type): Bool {
		throw "NYI!";
	}

	
	// Unification
	
	function strictUnifyWithType(type: Type) {
		return thisType.strictUnifyWithType(type);
	}


	// Generics

	// BUG: WHY DOES THIS NOT WORK CORRECTLY WITH SUBTYPING WTF
	function acceptsArgs(args: Array<Type>): Bool {
		if(args.length != params.length) return false;

		for(i in 0...args.length) {
			if(args[i] == null) return false;
			if(!args[i].hasParentType(params[i])) {
				return false;
			}
		}

		return true;
	}

	function applyArgs(args: Array<Type>): Null<Type> {
		if(args.length != params.length) return null;
		if(!this.acceptsArgs(args)) return null;

		final tctx: TypeVarCtx = [];
		final params2 = [];

		// Expand all typevars by binding the arg to the param
		for(i in 0...args.length) {
			args[i].bindTo(params[i], tctx)._match(
				at(type!) => params2.push(type),
				_ => return null
			);
		}

		return {t: TInstance(this, params2, tctx), span: null};
	}


	// Attributes

	function isNative(kind: NativeKind) return false;

	function isFlags() return false;
	
	function isStrong() return false;

	function isUncounted() return false;


	// Iterating

	function iterElemType(): Null<Type> {
		return refinees.findMap(ref ->
			// this allows us to obtain our typevars from refinee typevars
			(params.length == 0 ? ref.thisType : ref.applyArgs(params))._and(r =>
				r.iterElemType()._and(e => e.getFrom(thisType))
			)
		);
	}

	function iterAssocType(): Null<Tuple2<Type, Type>> {
		return refinees.findMap(ref ->
			// this allows us to obtain our typevars from refinee typevars
			(params.length == 0 ? ref.thisType : ref.applyArgs(params))._and(r =>
				r.iterAssocType()._match(
					at(null) => null,
					at({_1: k, _2: v}) => new Tuple2(k.getFrom(thisType), v.getFrom(thisType))
				)
			)
		);
	}


	// Effects tracking

	function trackEffectsIn(ctx: Ctx): Null<Effects> {
		return null;
	}

	function applyArgsTrackEffects(args: Array<Type>, ctx: Ctx): Null<Tuple2<Type, Effects>> {
		if(args.length != params.length) return null;

		var effects = Effects.empty;
		var tctx: TypeVarCtx = [];
		var params2 = [];

		// Expand all typevars by binding the arg to the param
		params._for(i => param, { final arg = args[i];
			arg.bindTo(param, tctx)._match(
				at(type!) => {
					params2.push(type);
				},
				_ => {
					return null;
				}
			);
		});

		// then gather up the effects from the expanded params
		var ctx2 = ctx.innerTypevars(tctx);
		for(type in params2) {
			type.trackEffectsIn(ctx2)._match(
				at(effects2!) => {
					effects += effects2;
				},
				_ => {
					return null;
				}
			);
		}
		
		return {
			_1: {t: TInstance(this, params2, tctx), span: null},
			_2: effects
		};
	}


	// Privacy

	function canSeeMember(member: Member) {
		return member.hidden._match(
			at(null) => true,
			at(_within!!) => {
				final within = _within._match(
					at(Some(t)) => t,
					at(None) => member.decl.thisType
				);

				within.hasChildDecl(this);
			}
		);
	}
	
	function canSeeMethod(method: AnyMethod) {
		return method.decl == this || method.hidden._match(
			at(null) => true,
			at(_within!!) => {
				final within = _within._match(
					at(Some(t)) => t,
					at(None) => method.decl.thisType
				);

				within.hasChildDecl(this);
			}
		);
	}


	// Members

	function instMembers(from: AnyTypeDecl): Array<Member> {
		return refinees.flatMap(r -> r.instMembers(from));
	}

	function findInstMember(ctx: Ctx, name: String, allowStatic = true, onlyParents = false): Null<MemberKind> {
		for(ref in refinees) {
			ref.findInstMember(ctx, name, allowStatic)._and(kind => {
				final tctx: TypeVarCtx = [];
				for(i => p in params) {
					p.bindTo(ref.params[i], tctx);
				}
				return MKFromRefinee(this, tctx, kind);
			});
		}

		return null;
	}


	// Method lookup

	// TODO: make sure parent methods don't collide with overridden or refined methods

	function findSingleStatic(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleStaticKind> {
		return null;
	}


	function findMultiStatic(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil): Array<MultiStaticKind> {
		return [];
	}


	function findSingleInst(ctx: Ctx, name: String, from: AnyTypeDecl, getter = false, cache: TypeCache = Nil): Null<SingleInstKind> {
		return null;
	}


	function findMultiInst(ctx: Ctx, names: Array<String>, from: AnyTypeDecl, setter = false, cache: TypeCache = Nil): Array<MultiInstKind> {
		return [];
	}


	function findCast(ctx: Ctx, target: Type, from: AnyTypeDecl, cache: TypeCache = Nil): Array<CastKind> {
		// TODO: fully implement
		target.t._match(
			at(TConcrete(sa is StrongAlias)) => {
				sa.type.strictUnifyWithType(thisType)._and(t => {
					return [CDowncast(target)];
				});
			},
			_ => {}
		);

		return [];
	}


	function findUnaryOp(ctx: Ctx, op: UnaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Null<UnaryOpKind> {
		return null;
	}


	function findBinaryOp(ctx: Ctx, op: BinaryOp, from: AnyTypeDecl, cache: TypeCache = Nil): Array<BinaryOpKind> {
		return [];
	}


	// Categories

	function findCategory(ctx: Ctx, cat: Type, forType: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		return lookup.findCategory(ctx, cat, forType, from, cache + thisType);
	}

	override function findThisCategory(ctx: Ctx, cat: Type, from: AnyTypeDecl, cache: Cache = Nil): Array<Category> {
		cache += thisType;

		var res = lookup.findCategory(ctx, cat, thisType, from, cache);

		for(ref in refinees) res = res.concat(ref.findThisCategory(ctx, cat, from, cache));

		return res;
	}
}