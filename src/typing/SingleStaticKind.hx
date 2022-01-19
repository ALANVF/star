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
	SSFromParent(parent: Type, kind: SingleStaticKind);
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
	at(SSFromTypevar(_, name, _, _)) => name,
	at(SSFromParent(_, kind)) => name(kind)
);

function retType(self: SingleStaticKind): Null<Type> return self._match(
	at(SSMethod(m)) => m.ret._or(Pass2.STD_Void.thisType),
	at(SSMultiMethod(m)) => m.ret._or(Pass2.STD_Void.thisType),
	at(SSInit({decl: d}) | SSMultiInit({decl: d})
	 | SSTaggedCase({decl: d}) | SSTaggedCaseAlias({decl: d})
	 | SSValueCase({decl: d})) => {t: TThis(d), span: null},
	at(SSMember(m)) => m.type,
	at(SSFromTypevar(_, _, _, _)) => null, // TODO
	at(SSFromParent(parent, kind)) => {
		retType(kind)._and(ret => {
			// TODO: make this smarter
			ret.t.match(TThis(_)) ? ret : ret.getFrom(parent.simplify());
		});
	}
);