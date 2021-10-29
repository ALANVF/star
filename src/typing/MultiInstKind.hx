package typing;

enum MultiInstKind {
	MIMethod(m: MultiMethod, ?partial: Bool);
	MIMember(m: Member);

	MIFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiInstKind);
}