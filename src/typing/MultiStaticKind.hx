package typing;

enum MultiStaticKind {
	MSMethod(m: MultiStaticMethod, ?partial: Array<Int>);
	MSInit(i: MultiInit, ?partial: Array<Int>);
	MSMemberwiseInit(ms: Array<Member>);
	MSMember(m: Member);
	MSTaggedCase(ms: Array<Member>, c: MultiTaggedCase);
	MSTaggedCaseAlias(c: TaggedCase);

	MSFromTypevar(tvar: TypeVar, names: Array<String>, setter: Bool, kind: MultiStaticKind);
}

function reduceBySender(kinds: Array<MultiStaticKind>) {
	if(kinds.length < 2) return kinds;

	// TODO: why doesn't mostSpecificBy work correctly?
	return Type.reduceOverloadsBy(kinds, kind -> kind._match(
		at(MSMethod(mth, _)) => mth.decl.thisType,
		at(MSInit(init, _)) => init.decl.thisType,
		at(MSMemberwiseInit(ms)) => ms[0].decl.thisType,
		at(MSMember(mem)) => mem.decl.thisType,
		at(MSTaggedCase(_, (_ : TaggedCase) => c) | MSTaggedCaseAlias(c)) => c.decl.thisType,
		at(MSFromTypevar(tvar, _, _, _)) => tvar.thisType
	));
}