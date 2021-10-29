package typing;

enum SingleInstKind {
	SIMethod(m: SingleMethod);
	SIMultiMethod(m: MultiMethod);
	SIMember(m: Member);

	SIFromTypevar(tvar: TypeVar, name: String, getter: Bool, kind: SingleInstKind);
}