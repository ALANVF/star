package typing;

@:using(typing.MemberKind)
enum MemberKind {
	MKMember(m: Member);
	
	MKFromTypevar(tvar: TypeVar, kind: MemberKind);
	MKFromParent(parent: Type, kind: MemberKind);
	MKFromRefinee(ref: TypeDecl, tctx: TypeVarCtx, kind: MemberKind);
}

function getMemberOwner(self: MemberKind) return self._match(
	at(MKMember(mem)) => mem.decl.thisType,
	at(MKFromTypevar(tvar, _)) => tvar.thisType,
	at(MKFromParent(_, kind2 = MKFromParent(_, _))) => getMemberOwner(kind2),
	at(MKFromParent(parent, _)) => parent.simplify(),
	at(MKFromRefinee(ref, _, _)) => ref.thisType
);

function getMember(self: MemberKind): Member return self._match(
	at(MKMember(mem)) => mem,
	at(MKFromTypevar(_, kind2)
	 | MKFromParent(_, kind2)
	 | MKFromRefinee(_, _, kind2)) => getMember(kind2)
);

function retType(self: MemberKind): Null<Type> return self._match(
	at(MKMember(m)) => m.type,
	at(MKFromTypevar(_, _)) => null, // TODO
	at(MKFromParent(parent, kind)) => {
		retType(kind)._and(ret => {
			// TODO: make this smarter
			ret.t.match(TThis(_)) ? ret : ret.getFrom(parent.simplify());
		});
	},
	at(MKFromRefinee(ref, tctx, kind)) => retType(kind)?.getInTCtx(tctx)
);