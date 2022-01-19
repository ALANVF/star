package util;

/*
// Better than using an anon structure because enums don't have virtual fields
private enum Dummy3<T, U, V> {
	Tuple3(t: T, u: U, v: V);
}

abstract Tuple3<T, U, V>(Dummy3<T, U, V>) {
	public var _1(get, never): T;
	public var _2(get, never): U;
	public var _3(get, never): V;
	
	public inline function new(a: T, b: U, c: V) this = Tuple3(a, b, c);
	
	private inline function get__1() {
		return switch this {case Tuple3(t, _, _): t;};
	}
	
	private inline function get__2() {
		return switch this {case Tuple3(_, u, _): u;};
	}
	
	private inline function get__3() {
		return switch this {case Tuple3(_, _, v): v;};
	}
}
*/

/*@:generic*/
@:structInit @:publicFields final class Tuple3<T, U, V> {
	final _1: T;
	final _2: U;
	final _3: V;

	inline function new(_1: T, _2: U, _3: V) {
		this._1 = _1;
		this._2 = _2;
		this._3 = _3;
	}
}