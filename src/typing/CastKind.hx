package typing;

enum CastKind {
	CMethod(m: CastMethod, ?tctx: TypeVarCtx);
	CUpcast(parent: Type);
	CDowncast(child: Type);
	CNative(t: Type);

	CFromTypevar(tvar: TypeVar, target: Type, kind: CastKind);
}

function reduceOverloads(kinds: Array<CastKind>, sender: Type, target: Type) {
	if(kinds.length == 0) return [];

	function loop(kind: CastKind) return kind._match(
		at(CFromTypevar(_, _, kind2)) => loop(kind2),
		_ => kind
	);

	// TODO: make this smarter

	final kinds2 = kinds.map(k -> loop(k));

	if(kinds2.some(k -> k.match(CMethod(_)))) {
		return kinds.filteriMap((k, i) -> kinds2[i]._match(
			at(k = CMethod(m)) => {
				final mtype = m.type.getFrom(sender);
				if(mtype.hasTypevars()) {
					final tctx: TypeVarCtx = [];
					if(target.bindTo(mtype, tctx) != null) {
						CMethod(m, tctx);
					} else {
						null;
					}
				} else {
					k;
				}
			},
			_ => null
		));
	} else {
		return kinds;
	}
}