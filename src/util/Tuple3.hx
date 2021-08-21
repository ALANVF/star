package util;

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