package text;

using util.Strings;
using hx.strings.Strings;

@:publicFields
private class _SourceFile {
	final path: String;
	final fullPath: String;
	final text: String;
	private var lineStarts: Array<Int> = [];
	
	var isReal(get, never): Bool;
	private function get_isReal() return sys.FileSystem.exists(fullPath);
	
	var lineCount(get, never): Int;
	private function get_lineCount() return lineStarts.length;
	
	function new(path, text) {
		this.path = path;
		fullPath = sys.FileSystem.fullPath(path);
		this.text = text;
		calculateLineStarts();
	}

	function line(index) {
		var startIndex = lineIndexToTextIndex(index);
		var endIndex = lineIndexToTextIndex(index + 1);
		return text.substr8(startIndex, endIndex - startIndex);
	}


	private function __compare(other: Dynamic):Int {
		final otherSrc = Std.downcast(other, _SourceFile);
		if(otherSrc != null) {
			return if(fullPath == otherSrc.fullPath) {
				0;
			} else {
				-1;
			}
		}

		return hl.Api.comparePointer(this, other);
	}

	private function lineIndexToTextIndex(index) return index >= lineStarts.length ? text.length8() : lineStarts[index];
	
	private function calculateLineStarts() {
		var cursor = new Cursor();
		var lastLine = 0;
		
		lineStarts.push(0);
		
		for(i in 0...text.length) {
			cursor.append(text.charAt8(i));

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

	public function iterator() return text.iterator();

	public function toString() return path;
}

@:forward(path, fullPath, text, isReal, lineCount, line, __compare, lineIndexToTextIndex, calculateLineStarts, iterator, toString)
abstract SourceFile(_SourceFile) {
	public inline function new(path, text) this = new _SourceFile(path, text);

	@:op(A == B)
	function equal(other: SourceFile) {
		return this.fullPath == other.fullPath;
	}
}