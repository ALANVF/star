package util.errors;

import haxe.*;

class IndexError extends PosException {
	public function new(?index: Int, ?previous: Exception, ?pos: PosInfos): Void {
		final msg = index == null
			? "Index out of bounds"
			: 'Index out of bounds: $index';
			
		super(msg, previous, pos);
	}
}