package typing;

import errors.Error;

interface IErrors {
	final errors: Array<Error>;

	function hasErrors(): Bool;
	function allErrors(): Array<Error>;
}