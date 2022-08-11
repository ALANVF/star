package typing;

import text.Span;

@:using(typing.LookupPath)
typedef LookupPath = List3<Null<Span>, String, Array<Type>>;


function simpleName(self: LookupPath) {
	return self.mapArray((_, n, p) -> n + (
		p.length == 0
			? ""
			: '[${p.joinMap(", ", t -> t.simpleName())}]'
	)).join(".");
}

function span(self: LookupPath) {
	return self._match(at([[s!, _, _], ..._]) => s, _ => throw "bad");
}