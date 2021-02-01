package util;

import haxe.ds.StringMap;
import haxe.ds.IntMap;
import hl.NativeArray;
import hl.types.ArrayObj;

@:publicFields
private class _Dict<K, V> {
	final keys: ArrayObj<K>;
	final values: ArrayObj<V>;

	function new(keys, values) {
		this.keys = keys;
		this.values = values;
	}
}

@:publicFields
abstract Dict<K, V>(_Dict<K, V>) {
	private function new(keys, values) {
		this = new _Dict(keys, values);
	}

	static inline function make() {
		return new Dict(new ArrayObj(), new ArrayObj());
	}

	static inline function alloc(size) {
		return new Dict(
			ArrayObj.alloc(new NativeArray(size)),
			ArrayObj.alloc(new NativeArray(size))
		);
	}

	@:from
	static function fromIntMap<V>(map: IntMap<V>): Dict<Int, V> {
		return @:privateAccess new Dict(
			ArrayObj.alloc(map.h.keysArray()),
			ArrayObj.alloc(map.h.valuesArray())
		);
	}

	@:from
	static function fromStringMap<V>(map: StringMap<V>): Dict<String, V> {
		return @:privateAccess new Dict(
			ArrayObj.alloc({
				final keys = map.h.keysArray();
				final size = map.size();
				final res = new NativeArray(size);
				for(i in 0...size) {
					res[i] = String.fromUCS2(keys[i]);
				}
				res;
			}),
			ArrayObj.alloc(map.h.valuesArray())
		);
	}
}