package util;

@:publicFields
class Strings {
	static inline function iterator(str: String) {
		return util.CharIterator.fromString(str);
	}
	
	static function escape(str: String, exceptWs = false, exceptQuote = false) {
		str = str.replaceAll("\\", "\\\\");
		if(!exceptQuote) str = str.replaceAll("\"", "\\\"");
		if(!exceptWs) str = str.replaceAll("\t", "\\t").replaceAll("\n", "\\n").replaceAll("\r", "\\r");
		// TODO: other control chars
		return str;
	}
}