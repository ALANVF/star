package util;

@:publicFields
class MathTools {
	static inline function min<T: Float>(a: T, b: T) return a <= b ? a : b;
	
	static inline function max<T: Float>(a: T, b: T) return a >= b ? a : b;
}