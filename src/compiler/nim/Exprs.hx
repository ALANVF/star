package compiler.nim;

@:using(compiler.nim.Exprs.ExprsTools)
typedef Exprs = Array<Expr>;

@:publicFields
class ExprsTools {
	static inline function toNim(self: Exprs, tabs = "", sep = ", ") return self.joinMap(sep, e -> e.toNim(tabs));
}