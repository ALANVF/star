package typing;

@:using(typing.SingleStaticKind)
enum SingleStaticKind {
	SSMethod(m: SingleStaticMethod);
	SSMultiMethod(m: MultiStaticMethod);
	SSInit(i: SingleInit);
	SSMultiInit(i: MultiInit);
	SSMember(m: Member);
	SSTaggedCase(c: SingleTaggedCase);
	SSTaggedCaseAlias(c: TaggedCase);
	SSValueCase(c: ValueCase);

	SSFromTypevar(tvar: TypeVar, name: String, getter: Bool, kind: SingleStaticKind);
}

function name(self: SingleStaticKind) return self._match(
	at(SSMethod(mth)) => mth.name.name,
	at(SSMultiMethod(mth)) => mth.params.find(p -> p.value == null).label.name,
	at(SSInit(init)) => init.name.name,
	at(SSMultiInit(init)) => init.params.find(p -> p.value == null).label.name,
	at(SSMember(mem)) => mem.name.name,
	at(SSTaggedCase(tcase)) => tcase.name.name,
	at(SSTaggedCaseAlias(tcase)) => tcase.assoc.value()._match(
		at(Single(_, _, n)) => n,
		_ => throw ""
	),
	at(SSValueCase(vcase)) => vcase.name.name,
	at(SSFromTypevar(_, name, _, _)) => name
);