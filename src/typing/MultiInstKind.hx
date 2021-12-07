package typing;

import typing.TypeVarCtx;

enum MultiInstKind {
	MIMethod(m: MultiMethod, ?partial: Array<Int>);
	MIMember(m: Member);

	MIFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiInstKind);
}

function reduceBySender(kinds: Array<MultiInstKind>) {
	if(kinds.length < 2) return kinds;

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(kinds, kind -> kind._match(
		at(MIMethod(mth, _)) => mth.decl.thisType,
		at(MIMember(mem)) => mem.decl.thisType,
		at(MIFromTypevar(tvar, _, _, _)) => tvar.thisType
	));
}

typedef InstOverload = {
	kind: MultiInstKind,
	tctx: Null<TypeVarCtx>,
	argTypes: Array<Null<Type>>,
	ret: Type,
	complete: Bool
};

function reduceOverloads(kinds: Array<MultiInstKind>, sender: Type, args: Array<TExpr>) {
	if(kinds.length == 0) return [];

	function loop(kind: MultiInstKind) return kind._match(
		at(MIFromTypevar(_, _, _, kind2)) => loop(kind2),
		_ => kind
	);

	var overloads: Array<InstOverload> = kinds.filterMap(function(kind) loop(kind)._match(
		at(MIMethod(mth, null)) => {
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
		at(MIFromTypevar(_, _, _, _)) => throw "bad"
	));

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
	return Type.reduceOverloadsBy(overloads, ov -> ov.kind._match(
		at(MIMethod(mth, _)) => mth.decl.thisType,
		at(MIMember(mem)) => mem.decl.thisType,
		at(MIFromTypevar(tvar, _, _, _)) => tvar.thisType
	));
}