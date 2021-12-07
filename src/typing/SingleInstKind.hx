package typing;

@:using(typing.SingleInstKind)
enum SingleInstKind {
	SIMethod(m: SingleMethod);
	SIMultiMethod(m: MultiMethod);
	SIMember(m: Member);

	SIFromTypevar(tvar: TypeVar, name: String, getter: Bool, kind: SingleInstKind);
}

function name(self: SingleInstKind) return self._match(
	at(SIMethod(mth)) => mth.name.name,
	at(SIMultiMethod(mth)) => mth.params.find(p -> p.value == null).label.name,
	at(SIMember(mem)) => mem.name.name,
	at(SIFromTypevar(_, name, _, _)) => name
);