package typing;

@:using(typing.TypeVarCtx)
typedef TypeVarCtx = Map<TypeVar, Type>;


function display(self: TypeVarCtx) {
	var res = "{";
	var first = true;
	for(tv => t in self) {
		if(first) {
			first = false;
		} else {
			res += ", ";
		}
		res += tv.name.name;
		res += " => ";
		res += t.fullName();
	}
	return res + "}";
}