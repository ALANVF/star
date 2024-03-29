package util.errors;

import haxe.*;

class Eof extends PosException {
	public function new(?message: String, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = message ?? "End of input reached";
		
		super(msg, previous, pos);
	}
}