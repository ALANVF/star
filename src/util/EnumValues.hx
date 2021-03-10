package util;

class EnumValues {
	// We are unable to access $setEnumField for some reason
#if hl
	@:hlNative("std", "enum_set_parameter")
	public static function setParameter<T: EnumValue>(e: T, index: Int, value: Dynamic) {}
#elseif macro
	public static function setParameter<T: EnumValue>(e: T, index: Int, value: Dynamic) {
		throw "Error!";
	}
#end
}