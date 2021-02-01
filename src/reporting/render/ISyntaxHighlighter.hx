package reporting.render;

import text.SourceFile;

interface ISyntaxHighlighter {
	function highlightLine(file: SourceFile, lineIndex: Int): Array<ColoredToken>;
}