package util;

#if (!macro && hl)
typedef BufferData = hl.Abstract<"hl_buffer">;

abstract Buffer(BufferData) {
	public var length(get, never): Int;

	public extern inline function new() {
		this = alloc();
	}

	@:hlNative("std", "buffer_val")
	public function add(value: Dynamic) {}

	@:hlNative("std", "buffer_char")
	public function addChar(char: Int) {}

	public inline function addString(str: String) {
		addBytes(@:privateAccess str.bytes);
	}

	public function toString() {
		var len = 0;
		final bytes = content(len);
		return @:privateAccess inline String.__alloc__(bytes, len);
	}

	@:hlNative("std", "buffer_alloc")
	private static function alloc(): BufferData {
		return null;
	}

	@:hlNative("std", "buffer_length")
	private function get_length(): Int {
		return 0;
	}

	@:hlNative("std", "buffer_content")
	private function content(len: hl.Ref<Int>): hl.Bytes {
		return null;
	}

	@:hlNative("std", "buffer_str")
	private function addBytes(bytes: hl.Bytes) {}
}
#else
typedef Buffer = StringBuf;
#end