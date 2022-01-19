package util;

import haxe.ds.Map;

@:publicFields
class Arrays {
	@:generic
	static function groupBy<T, U>(arr: Array<T>, fn: T -> U, ?equal: (U, U) -> Bool) {
		final found: Array<{key: U, values: Array<T>}> = [];
		final eq = equal == null ? ((l, r) -> l == r) : (equal : (U, U) -> Bool);

		for(i in 0...arr.length) {
			final elem = arr[i];
			final value = fn(elem);
			
			switch find(found, f -> eq(f.key, value)) {
				case null: found.push({key: value, values: [elem]});
				case {values: values}: values.push(elem);
			}
		}

		return [for(pair in found) pair.key => pair.values];
	}

	static function sorted<T>(arr: Array<T>, cmp: (T, T) -> Int) {
		final result = arr.copy();
		haxe.ds.ArraySort.sort(result, cmp);
		return result;
	}

	static function reversed<T>(array: Array<T>) {
		final result = array.copy();
		result.reverse();
		return result;
	}

	static inline function every<T>(array: Array<T>, callback: (currentValue: T) -> Bool) {
		var result = true;
		for(value in array) if(!callback(value)) {
			result = false;
			break;
		}
		return result;
	}
	
	static function everyi<T>(array: Array<T>, callback: (currentValue: T, index: Int) -> Bool) {
		for(i => value in array)
			if(!callback(value, i))
				return false;
		return true;
	}

	static function everyia<T>(array: Array<T>, callback: (currentValue: T, index: Int, array: Array<T>) -> Bool) {
		for(i => value in array)
			if(!callback(value, i, array))
				return false;
		return true;
	}

	static function every2<T, U>(array1: Array<T>, array2: Array<U>, callback: (value1: T, value2: U) -> Bool) {
		for(i => value in array1)
			if(!callback(value, array2[i]))
				return false;
		return true;
	}
	
	static function every2Strict<T, U>(array1: Array<T>, array2: Array<U>, callback: (value1: T, value2: U) -> Bool) {
		if(array1.length != array2.length) return false;
		
		for(i => value in array1)
			if(!callback(value, array2[i]))
				return false;
		return true;
	}


	static inline function some<T>(array: Array<T>, callback: (currentValue: T) -> Bool) {
		var result = false;
		for(value in array) if(callback(value)) {
			result = true;
			break;
		}
		return result;
	}
	
	static function somei<T>(array: Array<T>, callback: (currentValue: T, index: Int) -> Bool) {
		for(i => value in array)
			if(callback(value, i))
				return true;
		return false;
	}
	
	static function someia<T>(array: Array<T>, callback: (currentValue: T, index: Int, array: Array<T>) -> Bool) {
		for(i => value in array)
			if(callback(value, i, array))
				return true;
		return false;
	}

	static function none<T>(array: Array<T>, callback: (currentValue: T) -> Bool) {
		for(value in array)
			if(callback(value))
				return false;
		return true;
	}
	
	static function nonei<T>(array: Array<T>, callback: (currentValue: T, index: Int) -> Bool) {
		for(i => value in array)
			if(callback(value, i))
				return false;
		return true;
	}
	
	static function noneia<T>(array: Array<T>, callback: (currentValue: T, index: Int, array: Array<T>) -> Bool) {
		for(i => value in array)
			if(callback(value, i, array))
				return false;
		return true;
	}


	static function fill<T>(array: Array<T>, value: T, ?start: Int, ?end: Int) {
		final len = array.length;

		if(start == null) start = 1;
		if(end == null) end = len;

		final k = Std.int(if(start < 0) Math.max(len + start, 0) else Math.min(start, len));
		final finalValue = Std.int(if(end < 0) Math.max(len + end, 0) else Math.min(end, len));

		for(i in k...finalValue) {
			array[i] = value;
		}

		return array;
	}


	static function find<T>(array: Array<T>, callback: (element: T) -> Bool): Null<T> {
		for(value in array)
			if(callback(value))
				return value;
		return null;
	}
	
	static function findi<T>(array: Array<T>, callback: (element: T, index: Int) -> Bool): Null<T> {
		for(i => value in array)
			if(callback(value, i))
				return array[i];
		return null;
	}

	static function findia<T>(array: Array<T>, callback: (element: T, index: Int, array: Array<T>) -> Bool): Null<T> {
		for(i => value in array)
			if(callback(value, i, array))
				return array[i];
		return null;
	}


	static function findIndex<T>(array: Array<T>, callback: (element: T) -> Bool) {
		for(i => value in array)
			if(callback(value))
				return i;
		return -1;
	}
	
	static function findIndexi<T>(array: Array<T>, callback: (element: T, index: Int) -> Bool) {
		for(i => value in array)
			if(callback(value, i))
				return i;
		return -1;
	}

	static function findIndexia<T>(array: Array<T>, callback: (element: T, index: Int, array: Array<T>) -> Bool) {
		for(i => value in array)
			if(callback(value, i, array))
				return i;
		return -1;
	}


	static inline function count<T>(array: Array<T>, callback: (T) -> Bool) {
		var num = 0;
		for(value in array)
			if(callback(value))
				num++;
		return num;
	}


	static macro function _for<T>(array: ExprOf<Array<T>>, kv, body) {
		switch kv { case macro $k => $v:
			var dv = switch v {
				case {expr: EDisplay(v2, _)}: v2;
				default: v;
			};
			var vn = haxe.macro.ExprTools.toString(dv);
			return macro {
				for($k in 0...$array.length)
					$b{
						[macro final $vn = $array[$k]].concat(
							switch body {
								case macro $b{stmts}: stmts;
								default: [body];
							}
						)
					};
				
			};
			
		default: throw "error!"; }
	}
	
	static inline function forEach<T>(array: Array<T>, callback: (element: T) -> Void) {
		for(value in array)
			callback(value);
	}
	
	static inline function forEachi<T>(array: Array<T>, callback: (element: T, index: Int) -> Void) {
		for(i in 0...array.length)
			callback(array[i], i);
	}

	static inline function forEachia<T>(array: Array<T>, callback: (element: T, index: Int, array: Array<T>) -> Void) {
		for(i in 0...array.length)
			callback(array[i], i, array);
	}

	static function filteri<T>(array: Array<T>, callback: (element: T, index: Int) -> Bool): Array<T> {
		return [for(i => value in array)
			if(callback(value, i))
				value];
	}

	static function filteria<T>(array: Array<T>, callback: (element: T, index: Int, array: Array<T>) -> Bool): Array<T> {
		return [for(i => value in array)
			if(callback(value, i, array))
				value];
	}

	static function reduce<T>(array: Array<T>, callback: (prev: T, curr: T) -> T): Null<T> {
		var result: Null<T> = array[0];
		for(i in 1...array.length)
			result = callback(result, array[i]);
		return result;
	}

	static inline function fold<T, U>(array: Array<T>, initial: U, callback: (acc: U, curr: T) -> U): U {
		for(value in array)
			initial = callback(initial, value);
		return initial;
	}

	static inline function foldRight<T, U>(array: Array<T>, initial: U, callback: (acc: U, curr: T) -> U): U {
		var i = array.length;
		while(--i >= 0)
			initial = callback(initial, array[i]);
		return initial;
	}

	static function equals<T>(a1: Array<T>, a2: Array<T>) {
		return everyi(a1, (v, i) -> v == a2[i]);
	}

	static inline function zip<T, U, V>(a1: Array<T>, a2: Array<U>, callback: (e1: T, e2: U) -> V) {
		if(a1.length != a2.length) {
			throw "error!";
		}

		return [for(i => e1 in a1) callback(e1, a2[i])];
	}

	static function findMap<T, U>(array: Array<T>, callback: (element: T) -> Null<U>) {
		for(value in array) {
			final found = callback(value);

			if(found != null) {
				return found;
			}
		}

		return null;
	}

	static inline function filterMap<T, U>(array: Array<T>, callback: (element: T) -> Null<U>) {
		return [for(value in array) {
			final result = callback(value);

			if(result != null)
				(result : U);
		}];
	}

	static inline function flatMap<T, U>(array: Array<T>, callback: (element: T) -> Array<U>) {
		var result = [];

		for(value in array) {
			result = result.concat(callback(value));
		}

		return result;
	}

	static inline function tryMax<T: Float>(array: Array<T>): Null<T> {
		return reduce(array, cast Math.max);
	}

	static function max<T: Float>(array: Array<T>): T {
		final result = reduce(array, cast Math.max);
		if(result == null) {
			throw "No max value!";
		} else {
			return (result : T);
		}
	}

	static inline function last<T>(array: Array<T>): T {
		return array[array.length - 1];
	}
	
	static inline function setLast<T>(array: Array<T>, value: T) {
		array[array.length - 1] = value;
	}

	static inline function pushAll<T>(array: Array<T>, values: Array<T>) {
		for(i in 0...values.length) {
			array.push(values[i]);
		}
	}
	
	static function joinMap<T>(array: Array<T>, sep: String, fn: (T) -> String) {
		return switch array {
			case []: "";
			case [v]: fn(v);
			case [v1, v2]: fn(v1) + sep + fn(v2);
			default: {
				var res = fn(array[0]);
				
				for(i in 1...array.length) {
					res += sep;
					res += fn(array[i]);
				}
				
				res;
			}
		}
	}

	static extern inline overload function unique<T: EnumValue>(array: Array<T>) return _uniqueEnumValue(array);
	@:noUsing @:generic private static function _uniqueEnumValue<T: EnumValue>(array: Array<T>) {
		final res = [];

		for(i => value in array) {
			if(i > 0) {
				var found = false;
				for(j in 0...i) {
					if(array[j].equals(value)) {
						found = true;
						break;
					}
				}
				if(!found) res.push(value);
			} else {
				res.push(value);
			}
		}

		return res;
	}

	static extern inline overload function unique<T>(array: Array<T>) return _unique(array);
	@:noUsing @:generic private static function _unique<T>(array: Array<T>) {
		final res = [];

		for(i => value in array) {
			if(i > 0) {
				var found = false;
				for(j in 0...i) {
					if(array[j] == value) {
						found = true;
						break;
					}
				}
				if(!found) res.push(value);
			} else {
				res.push(value);
			}
		}

		return res;
	}


	@:generic static extern inline overload function isUnique<T: EnumValue>(array: Array<T>) return _isUniqueEnumValue(array);
	@:noUsing @:generic private static function _isUniqueEnumValue<T: EnumValue>(array: Array<T>) {
		for(i => value in array) {
			if(i > 0) {
				for(j in 0...i) {
					if(array[j].equals(value)) {
						return false;
					}
				}
			}
		}

		return true;
	}
	
	@:generic static extern inline overload function isUnique<T>(array: Array<T>) return _isUnique(array);
	@:noUsing @:generic private static function _isUnique<T>(array: Array<T>) {
		for(i => value in array) {
			if(i > 0) {
				for(j in 0...i) {
					if(array[j] == value) {
						return false;
					}
				}
			}
		}

		return true;
	}
}