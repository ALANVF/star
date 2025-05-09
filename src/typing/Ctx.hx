package typing;

import typing.Effects;
import typing.Traits;
import errors.Error;
import typing.Pass2;

enum Where {
	WEmptyMethod(m: EmptyMethod);
	WMethod(m: AnyMethod);
	WDecl(d: TypeDecl);
	WCategory(c: Category);
	WMember(m: Member);
	WTaggedCase(c: TaggedCase);
	WBlock;
	WBlockExpr;
	WPattern;
	WRecurse(lvars: Array<Tuple2<String, Local>>);
	WObjCascade(t: Null<Type>);
	WTypeCascade;
	WTypevars(typevars: TypeVarCtx);
}

@:publicFields @:structInit class Ctx {
	var where: Where;
	var outer: Null<Ctx> = null;
	var thisType: Type;
	var locals: Map<String, Local> = [];
	var labels: Map<String, TStmt> = [];
	var effects: Effects = Effects.empty;

	var typeDecl(get, never): AnyTypeDecl; private function get_typeDecl(): AnyTypeDecl return where._match(
		at(WDecl(decl)) => decl,
		at(WCategory(cat)) => cat,
		at(WEmptyMethod(m)) => m.decl,
		at(WMethod(m)) => m.decl,
		at(WMember(m)) => m.decl,
		at(WTaggedCase(c)) => c.decl,
		_ => outer?.typeDecl ?? throw "bad"
	);

	var thisLookup(get, never): ITypeLookupDecl; private function get_thisLookup(): ITypeLookupDecl return where._match(
		at(WDecl(decl)) => decl,
		at(WCategory(cat)) => cat,
		at(WMethod(m)) => m,
		at(WEmptyMethod(m)) => m.decl,
		at(WMember(m)) => m.decl,
		at(WTaggedCase(c)) => c.decl,
		_ => outer?.thisLookup ?? throw "bad"
	);

	var expectedReturnType(get, never): Null<Type>; private function get_expectedReturnType(): Null<Type> {
		return where._match(
			at(WDecl(_) | WCategory(_) | WBlockExpr) => null,
			at(WEmptyMethod(_) | WTaggedCase(_) | WObjCascade(_) | WTypeCascade) => STD_Void.thisType,
			at(WMember(mem)) => mem.type,
			at(WMethod(_ is Init)) => STD_Void.thisType,
			at(WMethod({type: ret} is CastMethod)) => ret,
			at(WMethod({ret: ret} is Method | {ret: ret} is StaticMethod | {ret: ret} is Operator)) => ret,
			at(WMethod(_)) => throw "bad",
			at(WBlock | WPattern | WRecurse(_) | WTypevars(_)) => outer?.expectedReturnType
		);
	}

	function innerDecl(decl: TypeDecl): Ctx {
		return {
			where: WDecl(decl),
			outer: this,
			thisType: decl.thisType
		};
	}

	function innerCategory(cat: Category): Ctx {
		return {
			where: WCategory(cat),
			outer: this,
			thisType: cat.thisType
		};
	}

	function innerEmptyMethod(method: EmptyMethod): Ctx {
		return {
			where: WEmptyMethod(method),
			outer: this,
			thisType: thisType
		};
	}

	function innerMethod(method: AnyMethod): Ctx {
		return {
			where: WMethod(method),
			outer: this,
			thisType: thisType
		};
	}

	function innerMember(member: Member): Ctx {
		return {
			where: WMember(member),
			outer: this,
			thisType: thisType
		};
	}

	function innerTaggedCase(tcase: TaggedCase): Ctx {
		return {
			where: WTaggedCase(tcase),
			outer: this,
			thisType: thisType
		};
	}

	function innerBlock(): Ctx {
		return {
			where: WBlock,
			outer: this,
			thisType: thisType
		};
	}

	function innerBlockExpr(): Ctx {
		return {
			where: WBlockExpr,
			outer: this,
			thisType: thisType
		};
	}

	function innerPattern(): Ctx {
		return {
			where: WPattern,
			outer: this,
			thisType: thisType
		};
	}

	function innerRecurse(lvars: Array<Tuple2<String, Local>>): Ctx {
		return {
			where: WRecurse(lvars),
			outer: this,
			thisType: thisType
		}
	}

	function innerCascade(?t: Type): Ctx {
		return {
			where: WObjCascade(t),
			outer: this,
			thisType: t ?? thisType // TODO: fix
		};
	}

	function innerTypeCascade(t: Type): Ctx {
		return {
			where: WTypeCascade,
			outer: this,
			thisType: t
		};
	}
	
	function innerTypevars(typevars: TypeVarCtx): Ctx {
		return {
			where: WTypevars(typevars),
			outer: this,
			thisType: thisType
		};
	}


	function addError(diag: Error) {
		switch where {
			case WEmptyMethod(method): method.errors.push(diag);
			case WMethod(method): method.errors.push(diag);
			case WDecl(decl): decl.errors.push(diag);
			case WCategory(cat): cat.errors.push(diag);
			default: outer._andOr(
				ctx => ctx.addError(diag),
				throw "bad"
			);
		}
	}

	function findLabel(label: String): Null<TStmt> {
		return labels[label]
			?? outer?.findLabel(label);
	}

	function findLocal(name: String, depth = 0): Null<Local> {
		return locals[name]._match(
			at(loc!, when(depth == 0)) => loc,
			_ => where._match(
				// use findInstMember?
				at(WObjCascade(t!)) => t.findSingleInst(this, name, outer.typeDecl, true)._match(
					at(SIMember(mem), when(depth == 0)) => new LocalField(this, mem, mem.name.name, mem.type, null),
					_ => outer.findLocal(name, depth)
				),
				at(WObjCascade(_)) => outer.findLocal(name, depth),
				at(WTypeCascade) => {
					// TODO
					outer.findLocal(name, depth);
				},
				at(WBlock | WBlockExpr) => outer.findLocal(name, depth),
				at(WPattern) => {
					// TODO
					outer.findLocal(name, depth);
				},
				at(WDecl(_)) => throw ":thonk:",
				at(WCategory(_)) => throw "bad",
				at(WEmptyMethod({decl: decl}) | WMethod({decl: decl}) | WMember({decl: decl}) | WTaggedCase({decl: decl})) => {
					// TODO: add proper check for statics?
					final isStatic = !allowsThis();
					decl.findInstMember(this, name)._and(kind => {
						final mem = kind.getMember();
						if(!mem.isStatic && isStatic) {
							null;
						} else {
							(outer.locals[name] = new LocalField(
								outer,
								mem,
								mem.name.name,
								kind.retType()._and(t => t.getIn(this)),
								mem.value._and(v => Pass2.typeExpr(this, v))
							));
						}
					});
				},
				at(WTypevars(_) | WRecurse(_)) => outer.findLocal(name, depth)
			)
		);
	}

	function getType(path: TypePath): Null<Type> {
		where._match(
			at(WObjCascade(t!)) =>
				t.findType(path.toLookupPath(thisLookup), Start, typeDecl, path.leadingCount())
				?? return outer.getType(path),
			at(WMethod(_) | WEmptyMethod(_) | WDecl(_) | WCategory(_)) =>
				thisLookup.findType(path.toLookupPath(thisLookup), Start, typeDecl, path.leadingCount()),
			_ => return outer.getType(path)
		)._match(
			at(t!) => return t.t._match(
				at(TApplied(t2, args)) => {
					final typeDecl_ = typeDecl;
					final thisLookup_ = thisLookup;
					{
						t: TApplied(t2, args.map(arg -> arg.t._match(
							at(TPath(depth, lookup, _)) => thisLookup_.findType(lookup, Start, typeDecl_, depth) ?? {
								addError(Type_InvalidTypeLookup(lookup.span(), 'Unknown type `${arg.simpleName()}`'));
								arg;
							},
							_ => arg
						))),
						span: t.span ?? t2.span
					};
				},
				_ => t
			),
			_ => outer._match(
				at(ctx!) => return ctx.getType(path),
				_ => {
					addError(Type_InvalidTypeLookup(path.span(), 'Unknown type `${path.simpleName()}`'));
					return null;
				}
			)
		);
	}

	function findTypevar(typevar: TypeVar): Null<Type> {
		return where._match(
			at(WTypevars(typevars)) => typevars[typevar],
			_ => outer?.findTypevar(typevar)
		);
	}

	function findTypevarOf(typevar: TypeVar): Null<TypeVar> {
		return where._match(
			at(WTypevars(typevars)) => {
				for(tvar => type in typevars) type.t._match(
					at(TTypeVar(typevar2)) => {
						if(typevar == typevar2) {
							return tvar;
						}
					},
					_ => {}
				);
				null;
			},
			_ => outer?.findTypevarOf(typevar)
		);
	}

	function allowsThis() {
		return switch where {
			case WDecl(d): !(d is Module);
			case WCategory(c): true;// meh
			case WEmptyMethod(m): !(m is StaticInit || m is StaticDeinit) && outer.allowsThis();
			case WMethod(m): !(m is StaticMethod) && outer.allowsThis();
			case WMember(m): !m.isStatic && outer.allowsThis();
			case WTaggedCase(_): true;
			case WBlock | WBlockExpr | WPattern | WRecurse(_): outer.allowsThis();
			case WObjCascade(_): true;
			case WTypeCascade: outer.allowsThis();
			case WTypevars(_): outer.allowsThis();
		};
	}

	function canAssignReadonlyField() {
		return where._match(
			at(WEmptyMethod(m)) => m is DefaultInit || m is StaticInit,
			at(WMethod(m)) => m is Init,
			at(WTaggedCase(_)) => true,
			at(WBlock | WPattern | WTypevars(_)) => outer?.canAssignReadonlyField() ?? false,
			_ => false
		);
	}

	function isPattern() {
		return where.match(WPattern);
	}

	function tryGetRecurse() {
		return where._match(
			at(WRecurse(lvars)) => lvars,
			at(WBlock | WBlockExpr | WPattern | WObjCascade(_) | WTypeCascade) => outer?.tryGetRecurse(),
			_ => null
		);
	}

	function description() {
		return where._match(
			at(WEmptyMethod(m)) => m.declName() + " for " + outer.description(),
			at(WMethod(m)) => m.declName() + " ["+m.methodName()+"] for "+outer.description(),
			at(WDecl(decl)) => decl.declName() + " " + decl.fullName(),
			at(WCategory(_)) => throw "bad",
			at(WMember(m)) => "member `"+m.name.name+"` for "+outer.description(),
			at(WTaggedCase(c)) => "tagged case [...] for "+outer.description(),
			at(WBlock | WBlockExpr) => "{ ... } in " + {
				final lookup = cast(this.thisLookup, IDecl);
				lookup.declName() + " " + lookup._match(
					at(mth is AnyMethod) =>"["+mth.methodName()+"] for "+mth.decl.declName()+" "+mth.decl.fullName(),
					_ => cast(lookup, AnyTypeDecl).fullName()
				);
			},
			at(WPattern) => "pattern ... in " + {
				final lookup = cast(this.thisLookup, IDecl);
				lookup.declName() + " " + lookup._match(
					at(mth is AnyMethod) => "["+mth.methodName()+"] for "+mth.decl.declName()+" "+mth.decl.fullName(),
					_ => cast(lookup, AnyTypeDecl).fullName()
				);
			},
			at(WRecurse(_)) => "recurse ... in " + outer.description(),
			at(WObjCascade(_)) => throw "todo",
			at(WTypeCascade) => throw "todo",
			at(WTypevars(_)) => outer.description()
		);
	}

	function addedEffectsFor(typevar: TypeVar) {
		return if(effects == Effects.empty) {
			outer?.addedEffectsFor(typevar) ?? new IEffects();
		} else {
			effects.addsFor(typevar);
		}
	}
}