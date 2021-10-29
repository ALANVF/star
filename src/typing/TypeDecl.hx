package typing;

import reporting.Diagnostic;
import text.Span;
import parsing.ast.Ident;
import typing.Traits;

@:build(util.Auto.build({keepInit: true}))
@:autoBuild(util.Auto.build())
abstract class TypeDecl implements IErrors {
	final errors: Array<Diagnostic> = [];
	final lookup: ILookupType;
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final span: Span;
	final name: Ident;
	var params: Array<Type>;
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];
	@ignore var thisType: Type;
	@ignore final refinements = new Array<TypeDecl>();
	@ignore final refinees = new Array<TypeDecl>();

	function new() {
		thisType = new Type(TConcrete(this));
	}
	
	abstract function declName(): String;

	function fullName(cache: List<Type> = Nil) {
		cache = cache.prepend(thisType);
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


	function isNative(kind: NativeKind) {
		return false;
	}


	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
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
				final res: Option<Type> = (search == Inside ? None : typevars.find(typeName).map(found -> found.filter(tvar ->
					!cache.contains(tvar.thisType)
					&& (tvar.params.length == 0 || tvar.params.length == args.length)
				)))._match(
					at(None | Some([])) => if(search == Inside) None else lookup.findType(path, Outside, from, depth, cache),
					at(Some(_), when(depth != 0)) => {
						if(search == Inside) {
							None;
						} else {
							lookup.findType(path, Outside, from, depth - 1, cache);
						}
					},
					at(Some([type])) => switch [args, type.params] {
						case [[], []]:
							finished = false;
							Some(type.thisType);
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
								Some({t: TApplied(type.thisType, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(Some(type)) => type,
										at(None) => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span});
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
								Some({t: TApplied(type, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(Some(type)) => type,
										at(None) => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span});
							case types:
								finished = false;
								Some({t: TApplied({t: TMulti(types), span: span}, args.map(arg -> arg.t._match(
									at(TPath(depth, lookup, source)) => source.findType(lookup, Start, from, depth)._match(
										at(Some(type)) => type,
										at(None) => {
											errors.push(Errors.invalidTypeLookup(span, 'Unknown type `${arg.simpleName()}`'));
											arg;
										}
									),
									_ => arg
								))), span: span});
						}
					}
				);
				
				switch [rest, res] {
					case [_, None]: lookup.findType(path, Outside, from, depth, cache);
					case [_, _] if(finished): res;
					case [Nil3, _]: res;
					case [_, Some({t: TConcrete(decl)})]: decl.findType(rest, Inside, from, 0, cache);
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
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
				at(Some({t: TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)})) => {
					if(this != decl
					&& name.name == decl.name.name
					&& lookup == decl.lookup
					&& params.every2Strict(decl.params, (p1, p2) -> p1.hasChildType(p2))
					&& !decl.refinements.contains(this)) {
						this.refinements.push(decl);
						decl.refinees.push(this);
					}
				},
				at(Some({t: TApplied({t: TMulti(types)}, _)})) => for(ty in types) ty.t._match(
					at(TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)) => {
						if(this != decl
						&& name.name == decl.name.name
						&& lookup == decl.lookup
						&& params.every2Strict(decl.params, (p1, p2) -> p1.hasChildType(p2))
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


	/*function unifyWithType(type: Type) {
		return thisType.unifyWithType(type);
	}*/
	
	
	function strictUnifyWithType(type: Type) {
		return thisType.strictUnifyWithType(type);
	}


	function canSeeMember(member: Member) {
		return member.hidden._match(
			at(None) => true,
			at(Some(_within)) => {
				final within = _within._match(
					at(Some(t)) => t,
					at(None) => member.lookup._match(
						at(td is TypeDecl) => td.thisType,
						at(cat is Category) => cat.type.orElseDo(
							cat.lookup._match(
								at(td is TypeDecl) => td.thisType,
								at(tv is TypeVar) => tv.thisType,
								_ => throw "bad"
							)
						),
						at(tv is TypeVar) => tv.thisType,
						_ => throw "???"
					)
				);

				within.hasChildDecl(this);
			}
		);
	}
	
	function canSeeMethod(method: AnyMethod) {
		return method.decl == this || method.hidden._match(
			at(None) => true,
			at(Some(_within)) => {
				final within = _within._match(
					at(Some(t)) => t,
					at(None) => method.decl._match(
						at(td is TypeDecl) => td.thisType,
						at(cat is Category) => cat.type.orElseDo(
							cat.lookup._match(
								at(td is TypeDecl) => td.thisType,
								at(tv is TypeVar) => tv.thisType,
								_ => throw "bad"
							)
						),
						at(tv is TypeVar) => tv.thisType,
						_ => throw "???"
					)
				);

				within.hasChildDecl(this);
			}
		);
	}


	function instMembers(from: ITypeDecl): Array<Member> {
		return refinees.flatMap(r -> r.instMembers(from));
	}


	// TODO: make sure parent methods don't collide with overridden or refined methods

	function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
		return null;
	}


	function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiStaticKind> {
		return [];
	}


	function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleInstKind> {
		return null;
	}


	function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiInstKind> {
		return [];
	}


	function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil): Array<CastKind> {
		return [];
	}


	function findUnaryOp(op: UnaryOp, from: ITypeDecl, cache: List<Type> = Nil): Null<UnaryOpKind> {
		return null;
	}


	function findBinaryOp(op: BinaryOp, from: ITypeDecl, cache: List<Type> = Nil): Array<BinaryOpKind> {
		return [];
	}


	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		return lookup.findCategory(cat, forType, from, cache.prepend(thisType));
	}

	function findThisCategory(cat: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		var res = lookup.findCategory(cat, thisType, from, cache.prepend(thisType));

		return res;
	}
}