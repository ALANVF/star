package typing;

typedef TypeMultiCandidate = {kind: MultiStaticKind, tctx: Null<TypeVarCtx>}

enum TypeMessage {
	Single(kind: SingleStaticKind);
	Multi(candidates: Array<TypeMultiCandidate>, labels: Array<String>, args: Array<TExpr>);

	Super(parent: Type, msg: TypeMessage);
}