package reporting.render;

@:publicFields
class NullSyntaxHighlighter implements ISyntaxHighlighter {
	function new() {}

	function highlightLine(file, lineIndex) {
		return [];
	}
}