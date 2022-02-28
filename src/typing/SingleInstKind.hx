package typing;

@:using(typing.SingleInstKind)
enum SingleInstKind {
	SIMethod(m: SingleMethod);
	SIMultiMethod(m: MultiMethod);
	SIMember(m: Member);

	SIFromTypevar(tvar: TypeVar, name: String, getter: Bool, kind: SingleInstKind);
	SIFromParent(parent: Type, kind: SingleInstKind);
}

function name(self: SingleInstKind) return self._match(
	at(SIMethod(mth)) => mth.name.name,
	at(SIMultiMethod(mth)) => mth.params.find(p -> p.value == null)._or(mth.params[0]).label.name,
	at(SIMember(mem)) => mem.name.name,
	at(SIFromTypevar(_, name, _, _)) => name,
	at(SIFromParent(_, kind)) => name(kind)
);

function retType(self: SingleInstKind): Null<Type> return self._match(
	at(SIMethod(m)) => m.ret._or(Pass2.STD_Void.thisType),
	at(SIMultiMethod(m)) => m.ret._or(Pass2.STD_Void.thisType),
	at(SIMember(m)) => m.type, // TODO: handle untyped members
	at(SIFromTypevar(_, _, _, kind)) => kind.retType(), // TODO
	at(SIFromParent(parent, kind)) => {
		retType(kind)._and(ret => {
			// TODO: make this smarter
			ret.t.match(TThis(_)) ? ret : ret.getFrom(parent.simplify());
		});
	}
);