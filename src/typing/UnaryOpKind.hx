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

function retType(self: UnaryOpKind): Null<Type> return self._match(
	at(UOMethod(m)) => m.ret ?? Pass2.STD_Void.thisType,
	at(UOFromTypevar(_, _, kind)) => kind.retType() // TODO
);

function isStep(self: UnaryOpKind) return digForMethod(self).op._match(at(Incr | Decr) => true, _ => false);