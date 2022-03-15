package text;

using util.Strings;

@:publicFields
class SourceFile {
	final path: String;
	final fullPath: String;
	final text: String;
	private var lineStarts: Array<Int> = [];
	
	var isReal(get, never): Bool;
	private inline function get_isReal() return sys.FileSystem.exists(fullPath);
	
	var lineCount(get, never): Int;
	private inline function get_lineCount() return lineStarts.length;
	
	function new(path, text: String) {
		this.path = path;
		fullPath = sys.FileSystem.fullPath(path);
		this.text = text.replaceAll("\r", ""); // LF makes life easier. CRLF does not
		calculateLineStarts();
	}

	function line(index) {
		var startIndex = lineIndexToTextIndex(index);
		var endIndex = lineIndexToTextIndex(index + 1);
		return inline text.substring8(startIndex, endIndex + 1);
	}


	private function __compare(other: Any) {
		return other._match(
			at(otherSrc is SourceFile) => if(fullPath == otherSrc.fullPath) 0 else -1,
			_ => hl.Api.comparePointer(this, other)
		);
	}

	private function lineIndexToTextIndex(index) return index >= lineStarts.length ? text.length8() : lineStarts[index];
	
	private function calculateLineStarts() {
		var cursor = new Cursor();
		var lastLine = 0;
		
		lineStarts.push(0);
		
		for(i in 0...text.length8()) {
			cursor.append(inline text.charAt8(i));

			if(cursor.column == 0) {
				if(cursor.line != lastLine) {
					lineStarts.push(i + 1);
					lastLine = cursor.line;
				} else {
					lineStarts[lineStarts.length - 1] = i + 1;
				}
			}
		}
	}

	public inline function iterator() return text.iterator();

	public function toString() return path;
}

/*@:forward(path, fullPath, text, isReal, lineCount, line, __compare, lineIndexToTextIndex, calculateLineStarts, iterator, toString)
abstract SourceFile(_SourceFile) {
	public inline function new(path, text) this = new _SourceFile(path, text);

	@:op(A == B)
	function equal(other: SourceFile) {
		return this.fullPath == other.fullPath;
	}
}*/