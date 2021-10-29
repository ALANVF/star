package typing;

enum UnaryOpKind {
	UOMethod(method: UnaryOperator);

	UOFromTypevar(tvar: TypeVar, op: UnaryOp, kind: UnaryOpKind);
}