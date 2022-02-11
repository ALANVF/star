package typing;

@:using(typing.TypeVarCtx)
typedef TypeVarCtx = Map<TypeVar, Type>;


function display(self: TypeVarCtx) {
	var res = "{\n  ";
	var first = true;
	for(tv => t in self) {
		if(first) {
			first = false;
		} else {
			res += ",\n  ";
		}
		res += tv.lookup._match(
			at(it is ITypeable) => it.fullName(),
			_ => Std.string(tv.lookup)
		);
		res += "#";
		res += tv.name.name;
		res += " => ";
		res += t.fullName();
	}
	return res + "\n}";
}