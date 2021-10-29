package typing;

enum BinaryOpKind {
	BOMethod(m: BinaryOperator);

	BOFromTypevar(tvar: TypeVar, op: BinaryOp, kind: BinaryOpKind);
}