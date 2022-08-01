package typing;

import text.Span;

interface IDecl extends IErrors {
	var span: Span;
	
	function declName(): String;
}