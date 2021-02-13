package typing;

import reporting.Diagnostic;

interface IErrors {
	final errors: Array<Diagnostic>;

	function hasErrors(): Bool;
	function allErrors(): Array<Diagnostic>;
}