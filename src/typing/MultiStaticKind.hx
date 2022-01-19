package typing;

enum MultiStaticKind {
	MSMethod(m: MultiStaticMethod, ?partial: Array<Int>);
	MSInit(i: MultiInit, ?partial: Array<Int>);
	MSMemberwiseInit(ms: Array<Member>);
	MSMember(m: Member);
	MSTaggedCase(ms: Array<Member>, c: MultiTaggedCase);
	MSTaggedCaseAlias(c: TaggedCase);

	MSFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiStaticKind);
	MSFromParent(parent: Type, kind: MultiStaticKind);
}

function getMethodOwner(kind: MultiStaticKind) return kind._match(
	at(MSMethod(mth, _)) => mth.decl.thisType,
	at(MSInit(init, _)) => init.decl.thisType,
	at(MSMemberwiseInit(ms)) => ms[0].decl.thisType,
	at(MSMember(mem)) => mem.decl.thisType,
	at(MSTaggedCase(_, (_ : TaggedCase) => c) | MSTaggedCaseAlias(c)) => c.decl.thisType,
	at(MSFromTypevar(tvar, _, _, _)) => tvar.thisType,
	at(MSFromParent(_, kind2 = MSFromParent(_, _))) => getMethodOwner(kind2),
	at(MSFromParent(parent, _)) => parent.simplify()
);

function reduceBySender(kinds: Array<MultiStaticKind>) {
	if(kinds.length < 2) return kinds;

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(kinds, getMethodOwner);
}

typedef StaticOverload = {
	kind: MultiStaticKind,
	tctx: Null<TypeVarCtx>,
	argTypes: Array<Null<Type>>,
	ret: Type,
	complete: Bool
};

function reduceOverloads(kinds: Array<MultiStaticKind>, sender: Type, names: Array<String>, args: Array<TExpr>) {
	if(kinds.length == 0) return [];

	function loop(kind: MultiStaticKind) return kind._match(
		at(MSFromTypevar(_, _, _, kind2)) => loop(kind2),
		_ => kind
	);
//trace(sender.fullName(), kinds);
	function filterOverload(kind): Null<StaticOverload> loop(kind)._match(
		at(MSMethod(mth, null)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			mth.params._for(i => param, {
				args[i].t._andOr(atype => {
					atype.getFrom(sender).bindTo(param.type.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
			});
			//trace(mth.methodName(), tctx, argTypes.map(a->a._and(aa=>aa.fullName())), complete);
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: mth.ret._andOr(
					ret => ret.getInTCtx(tctx).getFrom(sender).getInTCtx(tctx),
					({t: Pass2.STD_Void.thisType.t, span: mth.span} : Type)
				),
				complete: complete
			};
		},
		at(MSMethod(mth, indexes!!)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			indexes._for(i => paramIndex, {
				final param = mth.params[paramIndex];
				args[i].t._andOr(atype => {
					atype.getFrom(sender).bindTo(param.type.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
			});
			
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: mth.ret._andOr(
					ret => ret.getInTCtx(tctx).getFrom(sender),
					({t: Pass2.STD_Void.thisType.t, span: mth.span} : Type)
				),
				complete: complete
			};
		},
		at(MSInit(init, null)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			init.params._for(i => param, {
				args[i].t._andOr(atype => {
					atype.getFrom(sender).bindTo(param.type.simplify().getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						//trace(atype.fullName(), param.type.simplify().fullName());
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					//trace(args[i].orig.mainSpan().display());
					//trace(param.type.fullName());
					argTypes.push(null);
				});
			});
			//trace(init.methodName(), tctx, argTypes.map(a->a._and(aa=>aa.fullName())), complete);
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: sender.getInTCtx(tctx), // TODO: fix
				complete: complete
			};
		},
		at(MSInit(init, indexes!!)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			indexes._for(i => paramIndex, {
				final param = init.params[paramIndex];
				args[i].t._andOr(atype => {
					atype.getFrom(sender).bindTo(param.type.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
			});
			
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: sender.getInTCtx(tctx), // TODO: fix
				complete: complete
			};
		},
		at(MSMemberwiseInit(members)) => {
			// TODO: fix
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			members._for(i => mem, {
				mem.type._andOr(mtype => {
					args[i].t._andOr(atype => {
						atype.getFrom(sender).bindTo(mtype.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
							argTypes.push(atype2);
						}, {
							return null;
						});
					}, {
						complete = false;
						argTypes.push(null);
					});
				}, {
					trace('warning: null type for member `${mem.name.name}`');
					argTypes.push(null);
				});
			});
			
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: sender.getInTCtx(tctx), // TODO: fix
				complete: complete
			};
		},
		at(MSMember(mem)) => mem.type._match(
			at(mtype!) => {
				mtype = mtype.getFrom(sender);
				args[0].t._andOr(atype => {
					final tctx: TypeVarCtx = [];
					atype.getFrom(sender).bindTo(mtype, tctx)._andOr(atype2 => {
						return {
							kind: kind,
							tctx: tctx,
							argTypes: [atype2],
							ret: atype2,
							complete: true
						};
					}, {
						return null;
					});
				}, {
					return {
						kind: kind,
						tctx: null,
						argTypes: [null],
						ret: mtype,
						complete: false
					};
				});
			},
			_ => {
				trace("???");
				return null;
			}
		),
		at(MSTaggedCase([], tcase)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			tcase.params._for(i => p, {
				args[i].t._andOr(atype => {
					atype.getFrom(sender).bindTo(p.type.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
			});
			
			//trace(sender.simplify().getInTCtx(tctx).fullName());
			
			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: sender.getInTCtx(tctx), // TODO: fix
				complete: complete
			};
		},
		at(MSTaggedCase(members, tcase)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			var i = 0;
			while(i < members.length && members[i].name.name == names[i]) {
				final mem = members[i];
				mem.type._andOr(mtype => {
					args[i].t._andOr(atype => {
						atype.getFrom(sender).bindTo(mtype.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
							argTypes.push(atype2);
						}, {
							return null;
						});
					}, {
						complete = false;
						argTypes.push(null);
					});
				}, {
					trace('warning: null type for member `${mem.name.name}`');
					argTypes.push(null);
				});
				i++;
			}

			var i2 = i;
			for(p in tcase.params) {
				args[i2].t._andOr(atype => {
					atype.getFrom(sender).bindTo(p.type.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
				i2++;
			}
			
			while(i2 < args.length && i < members.length) {
				final mem = members[i];
				mem.type._andOr(mtype => {
					args[i2].t._andOr(atype => {
						atype.getFrom(sender).bindTo(mtype.getInTCtx(tctx).getFrom(sender), tctx)._andOr(atype2 => {
							argTypes.push(atype2);
						}, {
							return null;
						});
					}, {
						complete = false;
						argTypes.push(null);
					});
				}, {
					trace('warning: null type for member `${mem.name.name}`');
					argTypes.push(null);
				});
				i++;
				i2++;
			}

			return {
				kind: kind,
				tctx: tctx,
				argTypes: argTypes,
				ret: sender.getInTCtx(tctx), // TODO: fix
				complete: complete
			};
		},
		at(MSTaggedCaseAlias(tcase)) => {
			throw "todo";
		},
		at(MSFromTypevar(_, _, _, _)) => throw "bad",
		at(MSFromParent(parent, kind2)) => {
			// TODO
			return filterOverload(kind2)._and(res => {
				// TODO: make this smarter
				if(!res.ret.t.match(TThis(_))) res.ret = res.ret.getFrom(parent.simplify());
				res;
			});
		}
	);

	var overloads: Array<StaticOverload> = kinds.filterMap(filterOverload);

	if(overloads.length < 2) return overloads;

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(overloads, ov -> getMethodOwner(ov.kind));
}