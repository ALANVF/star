package util;

import haxe.macro.Context;

using haxe.macro.TypeTools;
using haxe.macro.ExprTools;

class Enums {
	public static macro function getIndex<T>(e: ExprOf<Enum<T>>, name) {
		return macro $v{Context.getType(e.toString()).getEnum().names.indexOf(name.toString())};
	}
}