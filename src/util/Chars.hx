package util;

@:publicFields
class Chars {
	static function escape(char: Char) {
		return if(char.isAsciiControl()) Util._match(char,
			at('\t'.code) => "\\t",
			at('\r'.code) => "\\r",
			at('\n'.code) => "\\n",
			_ => "\\x" + char.toInt().toHex(2)
		) else {
			char.toString();
		}
	}
}