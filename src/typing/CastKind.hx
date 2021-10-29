package typing;

enum CastKind {
	CMethod(m: CastMethod);
	CUpcast(parent: Type);
	CDowncast(child: Type);
	CNative(t: Type);

	CFromTypevar(tvar: TypeVar, target: Type, kind: CastKind);
}