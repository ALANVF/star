package reporting.render;

enum abstract TokenKind(Int) {
	var COMMENT;
	var KEYWORD;
	var LITERAL;
	var NAME;
	var PUNCTUATION;
	var OPERATOR;
	var OTHER;
}