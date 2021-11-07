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