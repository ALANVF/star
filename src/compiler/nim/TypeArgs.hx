package compiler.nim;

@:using(compiler.nim.TypeArgs.TypeArgsTools)
typedef TypeArgs = Array<{?n: Name, t: Type}>;

@:publicFields
class TypeArgsTools {
	static function toNim(self: TypeArgs, tabs = "", sep = ", ") {
		return "[" + self.joinMap(sep, t -> t._match(
			at({n: n!, t: t}) => n.toNim() + " = " + t.toNim(tabs),
			at({t: t}) => t.toNim(tabs)
		)) + "]";
	}
}