package util.errors;

import haxe.*;

class AssertionError extends PosException {
	public function new(?message: String, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = message == null
			? "Assertion failed"
			: 'Assertion failed: $message';
			
		super(msg, previous, pos);
	}
}