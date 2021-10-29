package typing;

enum MultiStaticKind {
	MSMethod(m: MultiStaticMethod, ?partial: Bool);
	MSInit(i: MultiInit, ?partial: Bool);
	MSMemberwiseInit(ms: Array<Member>);
	MSMember(m: Member);
	MSTaggedCase(ms: Array<Member>, c: MultiTaggedCase);
	MSTaggedCaseAlias(c: TaggedCase);

	MSFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiStaticKind);
}