package typing;

@:using(typing.UnaryOpKind)
enum UnaryOpKind {
	UOMethod(method: UnaryOperator);

	UOFromTypevar(tvar: TypeVar, op: UnaryOp, kind: UnaryOpKind);
}


function digForMethod(self: UnaryOpKind) return self._match(
	at(UOMethod(mth)) => mth,
	at(UOFromTypevar(_, _, kind)) => kind.digForMethod()
);