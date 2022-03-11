package typing;

typedef ObjMultiCandidate = {kind: MultiInstKind, ?tctx: TypeVarCtx}

enum ObjMessage {
	Lazy(msg: Message<TExpr>);
	Single(kind: SingleInstKind);
	Multi(candidates: Array<ObjMultiCandidate>, labels: Array<String>, args: Array<TExpr>);
	Cast(target: Type, candidates: Array<CastKind>);
	
	Super(parent: Type, msg: ObjMessage);
}