package typing;

enum TypeMessage {
	Single(kind: SingleStaticKind);
	Multi(candidates: Array<MultiStaticKind>, labels: Array<String>, args: Array<TExpr>);
}