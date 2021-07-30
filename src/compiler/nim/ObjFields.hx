package compiler.nim;

@:using(compiler.nim.ObjFields.ObjFieldsTools)
typedef ObjFields = Array<ObjField>;

final TAB = "    ";

@:publicFields
class ObjFieldsTools {
	static function toNim(self: ObjFields, tabs: String) {
		var res = "";
		
		tabs += TAB;
		
		final ws = '\n$tabs';
		
		for(field in self) {
			if(!field.match(FFields(_))) res += ws;
			res += field.toNim(tabs);
		}
		
		return res;
	}
}