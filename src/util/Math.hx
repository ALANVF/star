package util;

@:publicFields
class Math {
	static inline function min<T: Float>(a: T, b: T) return a <= b ? a : b;
	
	static inline function max<T: Float>(a: T, b: T) return a >= b ? a : b;
}