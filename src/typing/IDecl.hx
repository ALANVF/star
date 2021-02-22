package typing;

import text.Span;

typedef IDecl = IErrors & {
	final span: Span;
	
	function declName(): String;
}