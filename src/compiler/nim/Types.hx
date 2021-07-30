package compiler.nim;

@:using(compiler.nim.Types.TypesTools)
typedef Types = Array<Type>;

@:publicFields
class TypesTools {
	static inline function toNim(self: Types, tabs = "", sep = ", ") return self.joinMap(sep, t -> t.toNim(tabs));
}