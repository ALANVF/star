package compiler.nim;

@:using(compiler.nim.TypeParams.TypeParamsTools)
typedef TypeParams = Array<{?inType: Bool, n: Names, ?t: Type, ?d: Type}>;

@:publicFields
class TypeParamsTools {
	static function toNim(self: TypeParams, tabs: String) {
		return "[" + (self.joinMap("; ", param -> {
			var res = param.n.toNim();
			
			if(param.inType) res = 'in $res';
			
			param.t._and(t => {
				res += ": ";
				res += t.toNim(tabs);
			});
			
			param.d._and(d => {
				res += " = ";
				res += d.toNim(tabs);
			});
			
			return res;
		})) + "]";
	}
}