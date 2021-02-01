package util.errors;

import haxe.*;

class NullException extends PosException {
	public function new(?message: String, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = message == null
			? "Value was null"
			: message;
			
		super(msg, previous, pos);
	}
}