package typing;

enum CastKind {
	CMethod(m: CastMethod);
	CUpcast(parent: Type);
	CDowncast(child: Type);
	CNative(t: Type);

	CFromTypevar(tvar: TypeVar, target: Type, kind: CastKind);
}

function reduceOverloads(kinds: Array<CastKind>) {
	if(kinds.length == 0) return [];

	function loop(kind: CastKind) return kind._match(
		at(CFromTypevar(_, _, kind2)) => loop(kind2),
		_ => kind
	);

	// TODO: make this smarter

	final kinds2 = kinds.map(k -> loop(k));

	if(kinds2.some(k -> k.match(CMethod(_)))) {
		return kinds.filteri((k, i) -> kinds2[i].match(CMethod(_)));
	} else {
		return kinds;
	}
}