package typing;

import typing.Traits;

class Protocol extends ClassLike {
	final inits: Array<Init> = [];
	final operators: Array<Operator> = [];
	var defaultInit: Option<DefaultInit> = None;
	var deinit: Option<Deinit> = None;

	static function fromAST(lookup, ast: parsing.ast.decls.Protocol) {
		final protocol = new Protocol({
			lookup: lookup,
			span: ast.span,
			name: ast.name,
			params: []
		});

		for(typevar in ast.generics.mapArray(a -> TypeVar.fromAST(protocol, a))) {
			protocol.typevars.add(typevar.name.name, typevar);
		}

		if(ast.params.isSome()) {
			protocol.params = ast.params.value().of.map(param -> protocol.makeTypePath(param));
		}

		if(ast.parents.isSome()) {
			for(parent in ast.parents.value().parents) {
				protocol.parents.push(protocol.makeTypePath(parent));
			}
		}

		for(attr => span in ast.attrs) switch attr {
			case IsHidden(_) if(protocol.hidden.isSome()): protocol.errors.push(Errors.duplicateAttribute(protocol, ast.name.name, "hidden", span));
			case IsHidden(None): protocol.hidden = Some(None);
			case IsHidden(Some(outsideOf)): protocol.hidden = Some(Some(protocol.makeTypePath(outsideOf)));

			case IsFriend(_) if(protocol.friends.length != 0): protocol.errors.push(Errors.duplicateAttribute(protocol, ast.name.name, "friend", span));
			case IsFriend(One(friend)): protocol.friends.push(protocol.makeTypePath(friend));
			case IsFriend(Many(_, friends, _)): for(friend in friends) protocol.friends.push(protocol.makeTypePath(friend));

			case IsSealed(_) if(protocol.sealed.isSome()): protocol.errors.push(Errors.duplicateAttribute(protocol, ast.name.name, "sealed", span));
			case IsSealed(None): protocol.sealed = Some(None);
			case IsSealed(Some(outsideOf)): protocol.sealed = Some(Some(protocol.makeTypePath(outsideOf)));
		}

		for(decl in ast.body.of) switch decl {
			case DMember(m) if(m.attrs.exists(IsStatic)): protocol.staticMembers.push(Member.fromAST(protocol, m));
			case DMember(m): protocol.members.push(Member.fromAST(protocol, m));

			case DModule(m): protocol.addTypeDecl(Module.fromAST(protocol, m));

			case DClass(c): protocol.addTypeDecl(Class.fromAST(protocol, c));

			case DProtocol(p): protocol.addTypeDecl(Protocol.fromAST(protocol, p));
			
			case DKind(k): protocol.addTypeDecl(Kind.fromAST(protocol, k));

			case DAlias(a): protocol.addTypeDecl(Alias.fromAST(protocol, a));

			case DCategory(c): protocol.categories.push(Category.fromAST(protocol, c));

			case DMethod(m) if(m.attrs.exists(IsStatic)): StaticMethod.fromAST(protocol, m).forEach(x -> protocol.staticMethods.push(x));
			case DMethod(m): protocol.methods.push(Method.fromAST(protocol, m));

			case DInit(i): protocol.inits.push(Init.fromAST(protocol, i));

			case DOperator(o): Operator.fromAST(protocol, o).forEach(x -> protocol.operators.push(x));

			case DDefaultInit(i) if(protocol.staticInit.isSome()): protocol.staticInit = Some(StaticInit.fromAST(protocol, i));
			case DDefaultInit(i): protocol.defaultInit = Some(DefaultInit.fromAST(protocol, i));
			
			case DDeinit(d) if(protocol.staticDeinit.isSome()): protocol.staticDeinit = Some(StaticDeinit.fromAST(protocol, d));
			case DDeinit(d): protocol.deinit = Some(Deinit.fromAST(protocol, d));
			
			default: protocol.errors.push(Errors.unexpectedDecl(protocol, ast.name.name, decl));
		}

		return protocol;
	}

	override function hasErrors() {
		return super.hasErrors()
			|| members.some(m -> m.hasErrors())
			|| methods.some(m -> m.hasErrors())
			|| inits.some(i -> i.hasErrors())
			|| operators.some(o -> o.hasErrors());
	}

	override function allErrors() {
		var result = super.allErrors();
		
		for(member in members) result = result.concat(member.allErrors());
		for(method in methods) result = result.concat(method.allErrors());
		for(init in inits) result = result.concat(init.allErrors());
		for(op in operators) result = result.concat(op.allErrors());

		return result;
	}

	inline function declName() {
		return "class";
	}

	
	override function hasParentDecl(decl: TypeDecl) {
		//trace(this.fullName(),decl.fullName());
		return decl == Pass2.STD_Value || super.hasChildDecl(decl);
	}
	
	override function hasChildDecl(decl: TypeDecl) {
		//trace(this.fullName(),decl.fullName());
		return this == Pass2.STD_Value ? !decl.isNative(NVoid) : super.hasChildDecl(decl);
	}


	override function hasParentType(type: Type) {
		//trace(this.fullName(),type.fullName());
		return type == Pass2.STD_Value.thisType || super.hasParentType(type);
	}

	override function hasChildType(type: Type) {
		return this == Pass2.STD_Value ? !type.isNative(NVoid) : super.hasChildType(type);
	}


	override function defaultSingleStatic(name: String, from: ITypeDecl, getter = false) {
		if(this != Pass2.STD_Value && !getter) {
			return Pass2.STD_Value.findSingleStatic(name, from, getter);
		} else {
			return null;
		}
	}

	override function findSingleStatic(name: String, from: ITypeDecl, getter = false, cache: List<Type> = Nil): Null<SingleStaticKind> {
		if(cache.contains(thisType)) return null;
		
		if(!getter) {
			for(init in inits) init._match(
				at(si is SingleInit) => {
					if(si.name.name == name && from.canSeeMethod(si)) {
						return SSInit(si);
					}
				},
				_ => {}
			);
		}

		return super.findSingleStatic(name, from, getter, cache);
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

			// BAD
			if(candidates.length == 0 && !names.contains("_") && names.isUnique()) {
				final found = [];
				var bad = false;

				for(name in names) {
					this.members.find(mem -> mem.name.name == name)._match(
						at(mem!) => if(from.canSeeMember(mem)) {
							found.push(mem);
						} else {
							bad = true;
							break;
						},

						// TODO: dig through parent members
						_ => {
							bad = true;
							break;
						}
					);
				}

				if(!bad) {
					candidates.push(MSMemberwiseInit(found));
				}
			}
		}

		for(parent in parents) {
			candidates.pushAll(parent.findMultiStatic(names, from, setter, cache));
		}

		/*if(params.length != 0) {
			lookup.findType(List3.of([this.name.span, this.name.name, params]), false, cast cache.prepend(thisType))._match(
				at(Some({t: TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)})) => {
					candidates.pushAll(decl.findMultiStatic(names, from, setter, cache.prepend(thisType)));
				},
				at(Some({t: TMulti(types)})) => for(ty in types) ty.t._match(
					at(TConcrete(decl) | TApplied({t: TConcrete(decl)}, _)) => {
						candidates.pushAll(decl.findMultiStatic(names, from, setter, cache.prepend(thisType)));
					},
					_ => {}
				),
				_ => {}
			);
		}*/

		for(refinee in refinees) {
			candidates.pushAll(refinee.findMultiStatic(names, from, setter, cache));
		}

		return candidates;
	}


	override function defaultSingleInst(name: String, from: ITypeDecl, getter = false) {
		if(this != Pass2.STD_Value && !getter) {
			return Pass2.STD_Value.findSingleInst(name, from, getter);
		} else {
			return null;
		}
	}
}