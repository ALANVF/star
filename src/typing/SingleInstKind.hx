package typing;

enum SingleInstKind {
	SIMethod(m: SingleMethod);
	SIMember(m: Member);

	SIFromTypevar(tvar: TypeVar, name: String, getter: Bool, kind: SingleInstKind);
}