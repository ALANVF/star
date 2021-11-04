package typing;

import text.Span;

interface IDecl extends IErrors {
	final span: Span;
	
	function declName(): String;
}