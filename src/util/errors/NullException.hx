package util.errors;

import haxe.*;

class NullException extends PosException {
	public function new(?message: String, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = message ?? "Value was null";
		
		super(msg, previous, pos);
	}
}