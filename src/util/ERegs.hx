package util;

class ERegs {
#if hl
	@:noUsing
	@:hlNative("std", "regexp_matched_num")
	public static function matched_num(r: hl.Abstract<"ereg">): Int {
		return 0;
	}

	public static inline function matchedNum(r: EReg) {
		return matched_num(@:privateAccess r.r);
	}
#end
}