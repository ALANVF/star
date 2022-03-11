package typing;

enum MultiInstKind {
	MIMethod(m: MultiMethod, ?partial: Array<Int>);
	MIMember(m: Member);

	MIFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiInstKind);
	MIFromParent(parent: Type, kind: MultiInstKind);
}

function getMethodOwner(kind: MultiInstKind) return kind._match(
	at(MIMethod(mth, _)) => mth.decl.thisType,
	at(MIMember(mem)) => mem.decl.thisType,
	at(MIFromTypevar(tvar, _, _, _)) => tvar.thisType,
	at(MIFromParent(_, kind2 = MIFromParent(_, _))) => getMethodOwner(kind2),
	at(MIFromParent(parent, _)) => parent.simplify()
);

function reduceBySender(kinds: Array<MultiInstKind>) {
	if(kinds.length < 2) return kinds;

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(kinds, getMethodOwner);
}

typedef InstOverload = {
	kind: MultiInstKind,
	tctx: Null<TypeVarCtx>,
	argTypes: Array<Null<Type>>,
	ret: Type,
	complete: Bool
};

inline function simplify(overloads: Array<InstOverload>) {
	return overloads.map(ov -> {kind: ov.kind, tctx: ov.tctx});
}

function reduceOverloads(kinds: Array<MultiInstKind>, ctx: Ctx, sender: Type, args: Array<TExpr>) {
	if(kinds.length == 0) return [];

	function loop(kind: MultiInstKind) return kind._match(
		at(MIFromTypevar(_, _, _, kind2)) => loop(kind2),
		_ => kind
	);

	function filterOverload(kind): Null<InstOverload> loop(kind)._match(
		at(MIMethod(mth, null)) => {
			final argTypes: Array<Null<Type>> = [];
			final tctx: TypeVarCtx = [];
			var complete = true;

			mth.params._for(i => param, {
				args[i].t._andOr(atype => {
					final ptype = param.type/*.getInTCtx(tctx)*/.getFrom(sender);

					// WIP type inference for closures
					// Notes:
					/// fuck this shit
					args[i].e._match(
						at(EFunc(params, ret, body), when(args[i].orig.match(EAnonFunc(_, _, _)))) => {
							final ctx2 = ctx.innerBlock();
							final span = args[i].orig.nonNull().mainSpan();
							for(p in params) {
								ctx2.locals[p.name] = new Pass2.LocalParam(ctx2, span, p.name, p.type, null);
							}
							Pass2.getReturnType(ctx2, body)._match(
								at({complete: _, spans: _, incomplete: _, ret: ret2}) => {
									// TODO: stop being lazy
									atype.getFrom(sender).t._match(
										at(TMulti(types)) => {
											for(ty in types) ty.t._match(
												at(TInstance(fndecl, largs, ltctx)) => {
													//trace(ret2.fullName(),largs[0].fullName(), ltctx.display());
													final ltctx2: TypeVarCtx = [];
													ret2.bindTo(largs[0], [])._andOr(
														nret => {
															//largs[0] = nret;
															//trace(ret2.fullName(), largs[0].fullName(), nret.fullName());
															atype.bindTo(ptype, ltctx2);
															ptype.bindTo(atype, ltctx2);
															//trace(ltctx.display());
															//trace(ltctx2.display());
															for(tvar => mt in ltctx2) {
																if(!ltctx.exists(tvar)) {
																	//ltctx2.remove(tvar);
																} else {
																	ltctx[tvar] = mt;
																}
															}
															ptype.t._match(
																at(TInstance(decl, targs, ltctx3)) => {
																	for(tvar => mt in ltctx) {
																		ltctx[tvar] = mt.getInTCtx(ltctx3);
																	}
																	largs._for(i => a, {
																		largs[i] = a.getInTCtx(ltctx);
																	});
																	if({
																		var cond = false;
																		for(tv => mt in ltctx) {
																			if(mt.t.match(TTypeVar(_ == tv => true))) {
																				cond = true;
																				break;
																			}
																		}
																		cond;
																	}) {
																		//trace(largs.map(la->la.fullName()), ltctx.display());
																	}
																	else {
																		//trace(largs.map(la->la.fullName()), ltctx.display());
																		args[i].t=atype = {t: TInstance(decl, largs, ltctx), span: atype.span};
																	}
																},
																_ => throw "???"
															);
														}, {
															// TODO: what to do with this??
															trace("type mismatch???");
														}
													);
												},
												_ => {
													trace("???");
												}
											);
										},
										_ => {}
									);

									atype.getFrom(sender).bindTo(ptype, tctx)._andOr(atype2 => {
										args[i].t = atype2; // TODO: maybe wrong?
										argTypes.push(atype2);
										continue;
									}, {
										return null;
									});
								}
							);
						},
						_ => {}
					);
					
					atype.getFrom(sender).bindTo(ptype, tctx)._andOr(atype2 => {
						argTypes.push(atype2);
					}, {
						return null;
					});
				}, {
					complete = false;
					argTypes.push(null);
				});
			});

			/*mth.params._for(i => param, {
				argTypes[i]._and(atype => {
					atype.bindTo(param.type.getFrom(sender), tctx);
					//if(tctx.size() > 0) trace(tctx);
				});
			});*/

			/*if(tctx.size() == 0 && mth.typevars.size != 0 && mth.fuzzyName=="collect:") {
				final mp = mth.params.map(p->p.type.getFrom(sender));
				trace("");
				trace("#"+mth.methodName(),
					mth.typevars.allValues().map(tv -> tv.fullName()),
					mp
				);
				for(i=>a in argTypes) a._and(t => {
					trace(t.fullName());
					trace(mp[i].fullName());
					trace(t.bindTo(mp[i], tctx));
					trace(tctx.display());
				});
			}*/
			
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
		at(MIMethod(mth, indexes!!)) => {
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
		at(MIMember(mem)) => mem.type._match(
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
		at(MIFromTypevar(_, _, _, _)) => throw "bad",
		at(MIFromParent(parent, kind2)) => {
			// TODO
			return filterOverload(kind2)._and(res => {
				// TODO: make this smarter
				if(!res.ret.t.match(TThis(_))) res.ret = res.ret.getFrom(parent.simplify());
				res;
			});
		}
	);

	var overloads: Array<InstOverload> = kinds.filterMap(filterOverload);

	if(overloads.length < 2) return overloads;

	/*.println("\n===");
	for(ov in overloads) {
		Sys.println("");
		Sys.println("kind: "+ov.kind);
		ov.tctx._and(tctx => {
			Sys.println("tctx: [");
			for(k => v in tctx) {
				Sys.print("    ");
				Sys.print(k.fullName());
				Sys.print(" => ");
				Sys.println(v.fullName());
			}
			Sys.println("]");
		});
		Sys.println("argTypes: [");
		for(arg in ov.argTypes) {
			Sys.println("    "+arg._andOr(a => a.fullName(), "null"));
		}
		Sys.println("]");
		Sys.println("ret: " + ov.ret.fullName());
		Sys.println("complete: " + ov.complete);
	}*/

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(overloads, ov -> getMethodOwner(ov.kind));
}