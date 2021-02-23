package util.errors;

import haxe.*;

class KeyError extends PosException {
	public function new(?key: Any, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = key == null
			? "Invalid key"
			: 'Invalid key: $key';
			
		super(msg, previous, pos);
	}
}