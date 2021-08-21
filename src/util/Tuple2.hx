package util;

// Better than using an anon structure because enums don't have virtual fields
private enum Dummy2<T, U> {
	Tuple2(t: T, u: U);
}

abstract Tuple2<T, U>(Dummy2<T, U>) {
	public var _1(get, never): T;
	public var _2(get, never): U;
	
	public inline function new(a: T, b: U) this = Tuple2(a, b);
	
	private inline function get__1() {
		return switch this {case Tuple2(t, _): t;};
	}
	
	private inline function get__2() {
		return switch this {case Tuple2(_, u): u;};
	}
}