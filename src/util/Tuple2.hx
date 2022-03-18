package util;

@:generic
@:publicFields final class Tuple2<T, U> {
	final _1: T;
	final _2: U;

	inline function new(_1: T, _2: U) {
		this._1 = _1;
		this._2 = _2;
	}
}