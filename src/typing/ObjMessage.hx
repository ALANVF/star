package typing;

enum ObjMessage {
	Lazy(msg: Message<TExpr>);
	Single(kind: SingleInstKind);
	Multi(candidates: Array<MultiInstKind>, labels: Array<String>, args: Array<TExpr>);
	Cast(target: Type, candidates: Array<CastMethod>);
	
	Super(parent: Type, msg: ObjMessage);
}