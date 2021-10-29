package typing;

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