package typing;

import text.Span;
import parsing.ast.Ident;
import reporting.Diagnostic;
import typing.Traits;

@:build(util.Auto.build())
class Category implements IErrors {
	final errors: Array<Diagnostic> = [];
	@ignore final typevars = new MultiMap<String, TypeVar>();
	final lookup: ILookupType;
	final span: Span;
	final name: Ident;
	var path: Type;
	var type: Option<Type>;
	final staticMembers: Array<Member> = [];
	final staticMethods: Array<StaticMethod> = [];
	final methods: Array<Method> = [];
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var hidden: Option<Option<Type>> = None;
	final friends: Array<Type> = [];

	@ignore var thisType: Type;

	static function fromAST(lookup: ILookupType, ast: parsing.ast.decls.Category) {
		final category = new Category({
			lookup: lookup,
			span: ast.span,
			name: new Ident(ast.path.span(), ast.path.simpleName()),
			path: null, // hack for partial initialization
			type: null  // hack for partial initialization
		});

		var path = (ast.path : TypePath).toType(category);

		category.path = path;
		category.type = ast.type.map(x -> category.makeTypePath(x));

		category.thisType = switch category.type {
			case Some(t): t;
			case None: (cast lookup : ITypeDecl).thisType;
		};

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(category, a))) {
			category.typevars.add(typevar.name.name, typevar);
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(category.hidden.isSome()): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "hidden", span));
			case IsHidden(None): category.hidden = Some(None);
			case IsHidden(Some(outsideOf)): category.hidden = Some(Some(category.makeTypePath(outsideOf)));

			case IsFriend(_) if(category.friends.length != 0): category.errors.push(Errors.duplicateAttribute(category, category.name.name, "friend", span));
			case IsFriend(One(friend)): category.friends.push(category.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) category.friends.push(category.makeTypePath(friend));
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): category.staticMembers.push(Member.fromAST(category, m));
			
			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(category, m).forEach(x -> category.staticMethods.push(x));
			case DMethod(m): category.methods.push(Method.fromAST(category, m));

			case DInit(i): category.inits.push(Init.fromAST(category, i));

			case DOperator(o): Operator.fromAST(category, o).forEach(x -> category.operators.push(x));

			default: category.errors.push(Errors.unexpectedDecl(category, category.name.name, decl));
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

	inline function declName() {
		return "category";
	}

	function fullName() {
		return type.doOrElse(t => t.fullName(), lookup._match(
			at(decl is TypeDecl) => decl.fullName(),
			at(tvar is TypeVar) => tvar.fullName(),
			_ => throw "???"
		)) + "+" + path.fullName();
	}


	function findType(path: LookupPath, search: Search, from: Null<ITypeDecl>, depth = 0, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) return None;
		//cache = cache.prepend(thisType);

		if(search == Inside) return None;

		return path._match(
			at([[span, "This", args]], when(depth == 0)) => {
				if(args.length == 0) {
					type.doOrElse(
						t => Some(t),
						lookup.findType(path, Start, from, 0, cache)
					);
				} else {
					// prob shouldn't be attatched to *this* category decl, but eh
					errors.push(Errors.notYetImplemented(span));
					None;
				}
			},
			at([[span, typeName, args], ...rest]) => {
				var finished = true;
				final res: Option<Type> = typevars.find(typeName).map(found -> found.filter(tvar ->
					!cache.contains(tvar.thisType)
					&& (tvar.params.length == 0 || tvar.params.length == args.length)
				))._match(
					at(None | Some([])) => lookup.findType(path, Outside, this, depth, cache).orDo(
						this.thisType.findType(path, Start, this, depth, cache)
					),
					at(Some(_), when(depth != 0)) => lookup.findType(path, Outside, this, depth - 1, cache).orDo(
						this.thisType.findType(path, Start, this, depth - 1, cache)
					),
					at(Some([tvar])) => switch [args, tvar.params] {
						case [[], _]:
							finished = false;
							Some({t: tvar.thisType.t, span: span}); // should probably curry parametrics but eh
						case [_, []]:
							// should this check for type aliases?
							// error...?
							None;
						case [_, params]:
							if(args.length > params.length) {
								errors.push(Errors.invalidTypeApply(span, "Too many arguments"));
								None;
							} else if(args.length < params.length) {
								errors.push(Errors.invalidTypeApply(span, "Not enough arguments"));
								None;
							} else {
								finished = false;
								Some({t: TApplied(tvar.thisType, args), span: span});
							}
					},
					at(Some(found)) => {
						if(args.length == 0) {
							finished = false;
							Some({t: TMulti(found.map(t -> t.thisType)), span: span});
						} else switch found.map(t -> t.thisType) {
							case []:
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [tvar]:
								finished = false;
								Some({t: TApplied(tvar, args), span: span});
							case tvars:
								finished = false;
								Some({t: TMulti(tvars), span: span});
						}
					}
				);

				switch [rest, res] {
					case [_, None]: lookup.findType(path, Outside, from, depth, cache);
					case [_, _] if(finished): res;
					case [Nil3, _]: res;
					case [_, Some({t: TConcrete(decl)})]: decl.findType(rest, Outside, from, 0, cache);
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
				}
			},
			_ => throw "bad"
		);
	}

	/*function findTypeOld(path: LookupPath, absolute = false, cache: List<{}> = Nil): Option<Type> {
		if(cache.contains(this)) {
			return None;
		} else {
			cache = cache.prepend(this);
		}

		return path._match(
			at([[_, "This", []]], when(absolute)) => type.doOrElse(
				t => Some(t),
				lookup.findTypeOld(path, true, cache)
			),
			at([[span, "This", _]], when(absolute)) => {
				// prob shouldn't be attatched to *this* category decl, but eh
				errors.push(Errors.notYetImplemented(span));
				None;
			},
			at([[span, typeName, args], ...rest]) => {
				final res: Option<Type> = switch typevars.find(typeName) {
					case None: return if(absolute) lookup.findTypeOld(path, true, cache) else None;
					case Some([type]):
						switch [args, type.params] {
							case [[], _]: Some({t: type.thisType.t, span: span}); // should probably curry parametrics but eh
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
								errors.push(Errors.invalidTypeApply(span, "No candidate matches the type arguments"));
								None;
							case [type]: Some({t: TApplied(type, args), span: span});
							case types: Some({t: TMulti(types), span: span});
						}
				};

				switch [rest, res] {
					case [Nil3, _]: res;
					case [_, Some(type)]: Some({t: TLookup(type, rest, this), span: span});
					case [_, None]: res;
				}
			},
			_ => if(absolute) lookup.findTypeOld(path, true, cache) else None
		);
	}*/

	function makeTypePath(path: TypePath) {
		return path.toType(this);
	}


	function hasParentDecl(decl: TypeDecl) {
		return switch type {
			case Some(t): t.hasParentDecl(decl);
			case None: lookup._match(
				at(td is TypeDecl) => td.hasParentDecl(decl),
				at(tv is TypeVar) => tv.hasParentDecl(decl),
				_ => false
			);
		}
	}

	function hasChildDecl(decl: TypeDecl) {
		return switch type {
			case Some(t): t.hasChildDecl(decl);
			case None: lookup._match(
				at(td is TypeDecl) => td.hasChildDecl(decl),
				at(tv is TypeVar) => tv.hasChildDecl(decl),
				_ => false
			);
		}
	}

	function hasParentType(type2: Type) {
		return switch type {
			case Some(t): t.hasParentType(type2);
			case None: lookup._match(
				at(td is TypeDecl) => td.hasParentType(type2),
				at(tv is TypeVar) => tv.hasParentType(type2),
				_ => false
			);
		}
	}

	function hasChildType(type2: Type) {
		return switch type {
			case Some(t): t.hasChildType(type2);
			case None: lookup._match(
				at(td is TypeDecl) => td.hasChildType(type2),
				at(tv is TypeVar) => tv.hasChildType(type2),
				_ => false
			);
		}
	}


	function canSeeMember(member: Member) {
		return member.lookup == this
			|| thisType.canSeeMember(member);
	}

	function canSeeMethod(method: AnyMethod) {
		return method.decl == this
			|| thisType.canSeeMethod(method);
	}


	function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
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

	function findMultiStatic(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiStaticKind> {
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
				at(mm is MultiStaticMethod) => {
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

			for(init in inits) init._match(
				at(mi is MultiInit) => if(from.canSeeMethod(mi)) {
					if(mi.params.every2Strict(names, (l, n) -> l.label.name == n)) {
						candidates.push(MSInit(mi));
					} else if(names.length < mi.params.length) {
						var n = 0;
						var p = 0;
						var matchedOnce = false;
						while(n < names.length && p < mi.params.length) {
							mi.params[p]._match(
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
							candidates.push(MSInit(mi, true));
						}
					}
				},
				_ => {}
			);
		}

		return candidates;
	}


	function findSingleInst(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleInstKind> {
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
			_ => {}
		);

		return null;
	}

	function findMultiInst(names: Array<String>, from: ITypeDecl, setter = false, cache: List<Type> = Nil): Array<MultiInstKind> {
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
				at(mm is MultiMethod) => {
					if(mm.params.every2Strict(names, (l, n) -> (n == "=" && mm.isSetter) || l.label.name == n)) {
						candidates.push(MIMethod(mm));
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
							candidates.push(MIMethod(mm, true));
						}
					}
				},
				_ => {}
			);
		}

		return candidates;
	}


	function findCast(target: Type, from: ITypeDecl, cache: List<Type> = Nil): Array<CastMethod> {
		//if(type.exists(t -> cache.contains(t)) || lookup._match(at(d is TypeDecl) => cache.contains(d.thisType), _ => false)) return null;

		final candidates: Array<CastMethod> = [];

		/*for(mem in members) {
			if(mem.matchesSetter(name) && from.canSeeMember(mem)) {
				return MIMember(mem);
			}
		}*/

		for(mth in methods) mth._match(
			at(cm is CastMethod) => {
				if(cm.type.hasChildType(target)) {
					candidates.push(cm);
				}
			},
			_ => {}
		);

		return candidates;
	}


	function findCategory(cat: Type, forType: Type, from: ITypeDecl, cache: List<{}> = Nil): Array<Category> {
		final found = lookup.findCategory(cat, forType, from, cache);
		
		if(thisType.hasChildType(forType) && path.hasChildType(cat)) {
			return found.concat([this]);
		} else {
			return found;
		}
	}
}