package util;

@:publicFields
class HLSys {
	static final WIN_UTF8 = 1;
	static final AUTO_FLUSH = 2;

	@:hlNative("std", "sys_set_flags")
	static function setFlags(flags: Int): Int {
		return 0;
	}
}