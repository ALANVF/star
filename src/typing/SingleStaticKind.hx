package typing;

enum SingleStaticKind {
	SSMethod(m: SingleStaticMethod);
	SSInit(i: SingleInit);
	SSMember(m: Member);
	SSTaggedCase(c: SingleTaggedCase);
	SSTaggedCaseAlias(c: TaggedCase);
	SSValueCase(c: ValueCase);
}