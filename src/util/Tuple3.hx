package util;

@:generic
@:publicFields final class Tuple3<T, U, V> {
	final _1: T;
	final _2: U;
	final _3: V;

	inline function new(_1: T, _2: U, _3: V) {
		this._1 = _1;
		this._2 = _2;
		this._3 = _3;
	}
}