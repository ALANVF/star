package typing;

@:using(typing.BinaryOpKind)
enum BinaryOpKind {
	BOMethod(m: BinaryOperator);

	BOFromTypevar(tvar: TypeVar, op: BinaryOp, kind: BinaryOpKind);
}

function digForMethod(self: BinaryOpKind) return self._match(
	at(BOMethod(mth)) => mth,
	at(BOFromTypevar(_, _, kind)) => kind.digForMethod()
);

function getMethodOwner(self: BinaryOpKind) return self._match(
	at(BOMethod(mth)) => mth.decl.thisType,
	at(BOFromTypevar(tvar, _, _)) => tvar.thisType
);

typedef BinaryOverload = {
	kind: BinaryOpKind,
	tctx: Null<TypeVarCtx>,
	argType: Null<Type>,
	ret: Type,
	complete: Bool
};

inline function simplify(overloads: Array<BinaryOverload>) {
	return overloads.map(ov -> {kind: ov.kind, tctx: ov.tctx});
}

function reduceOverloads(kinds: Array<BinaryOpKind>, sender: Type, rhs: TExpr) {
	//if(kinds.length == 0) return [];

	function loop(kind: BinaryOpKind) return kind._match(
		at(BOFromTypevar(_, _, kind2)) => loop(kind2),
		_ => kind
	);

	function filterOverload(kind): Null<BinaryOverload> return loop(kind)._match(
		at(BOMethod(mth)) => {
			/*if(mth.body == null && mth.native == null) {
				return null;
			}*/

			var argType: Null<Type> = null;
			final tctx: TypeVarCtx = [];
			var complete = true;

			rhs.t._andOr(rtype => {
				/*trace(sender.fullName(), rtype.fullName(), mth.paramType.getFrom(sender).fullName());
				trace(rhs.orig.nonNull().mainSpan().display());
				trace(mth.paramType.span.display());*/
				rtype.bindTo(mth.paramType.getFrom(sender), tctx)._andOr(rtype2 => {
					argType = rtype2;
				}, {
					return null;
				});
			}, {
				complete = false;
				argType = null;
			});

			return {
				kind: kind,
				tctx: tctx,
				argType: argType,
				ret: mth.ret._andOr(
					ret => ret.getInTCtx(tctx).getFrom(sender).getInTCtx(tctx),
					({t: Pass2.STD_Void.thisType.t, span: mth.span} : Type)
				),
				complete: complete
			}
		},
		at(BOFromTypevar(_, _, _)) => throw "bad"
	);

	var overloads: Array<BinaryOverload> = kinds.filterMap(filterOverload);

	if(overloads.length < 2) return overloads;

	// TODO: why doesn't mostSpecificBy work correctly?
	final res = Type.reduceOverloadsBy(overloads, ov -> getMethodOwner(ov.kind));
	final res2 = res.filter(ov -> ov.kind._match(
		at(BOMethod(mth)) => !(mth.body == null && mth.native == null),
		_ => true // TODO
	));

	if(res2.length == 0) {
		return res;
	} else {
		return res2;
	}
}