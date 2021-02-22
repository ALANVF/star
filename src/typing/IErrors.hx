package typing;

import reporting.Diagnostic;

typedef IErrors = {
	final errors: Array<Diagnostic>;

	function hasErrors(): Bool;
	function allErrors(): Array<Diagnostic>;
}