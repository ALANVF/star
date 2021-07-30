package compiler.nim;

@:using(compiler.nim.Names.NamesTools)
typedef Names = Array<Name>;

@:publicFields
class NamesTools {
	static inline function toNim(self: Names) return self.joinMap(", ", n -> n.toNim());
}