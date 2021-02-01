package util.errors;

import haxe.*;

class Eof extends PosException {
	public function new(?message: String, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = message == null
			? "End of input reached"
			: message;
			
		super(msg, previous, pos);
	}
}