package util;

import haxe.ds.IntMap;
import haxe.ds.StringMap;
import haxe.ds.ObjectMap;
import haxe.ds.EnumValueMap;

abstract class BaseMultiMap<K, V> {
	public var size(get, never): Int;
	abstract function get_size(): Int;

	public inline function new() {}

	abstract function makeNew(): BaseMultiMap<K, V>;

	public abstract function get(k: K): Null<Array<V>>;
	public abstract function set(k: K, v: Array<V>): Void;
	public abstract function exists(k: K): Bool;
	public abstract function remove(k: K): Bool;
	public abstract function keys(): Iterator<K>;
	public abstract function iterator(): Iterator<Array<V>>;
	public abstract function keyValueIterator(): KeyValueIterator<K, Array<V>>;
	public inline function copy() {
		final res = this.makeNew();
		for(key in this.keys()) res.set(key, this.get(key).copy());
		return res;
	}
	public abstract function toString(): String;
	public abstract function clear(): Void;
}

class IntMultiMap<V> extends BaseMultiMap<Int, V>  {
	final map = new IntMap<Array<V>>();

	inline function get_size() return Maps.size(map);

	inline function makeNew() return new IntMultiMap<V>();

	public inline function get(k) return map.get(k);
	public inline function set(k, v) return map.set(k, v);
	public inline function exists(k) return map.exists(k);
	public inline function remove(k) return map.remove(k);
	public inline function keys() return map.keys();
	public inline function iterator() return map.iterator();
	public inline function keyValueIterator() return map.keyValueIterator();
	public inline function toString() return map.toString();
	public inline function clear() return map.clear();
}

class StringMultiMap<V> extends BaseMultiMap<String, V> {
	final map = new StringMap<Array<V>>();

	inline function get_size() return Maps.size(map);

	inline function makeNew() return new StringMultiMap<V>();

	public inline function get(k) return map.get(k);
	public inline function set(k, v) return map.set(k, v);
	public inline function exists(k) return map.exists(k);
	public inline function remove(k) return map.remove(k);
	public inline function keys() return map.keys();
	public inline function iterator() return map.iterator();
	public inline function keyValueIterator() return map.keyValueIterator();
	public inline function toString() return map.toString();
	public inline function clear() return map.clear();
}

class ObjectMultiMap<K: {}, V> extends BaseMultiMap<K, V> {
	final map = new ObjectMap<K, Array<V>>();

	inline function get_size() return Maps.size(map);

	inline function makeNew() return new ObjectMultiMap<K, V>();

	public inline function get(k) return map.get(k);
	public inline function set(k, v) return map.set(k, v);
	public inline function exists(k) return map.exists(k);
	public inline function remove(k) return map.remove(k);
	public inline function keys() return map.keys();
	public inline function iterator() return map.iterator();
	public inline function keyValueIterator() return map.keyValueIterator();
	public inline function toString() return map.toString();
	public inline function clear() return map.clear();
}

class EnumValueMultiMap<K: EnumValue, V> extends BaseMultiMap<K, V> {
	final map = new EnumValueMap<K, Array<V>>();

	inline function get_size() return Maps.size(map);

	inline function makeNew() return new EnumValueMultiMap<K, V>();

	public inline function get(k) return map.get(k);
	public inline function set(k, v) return map.set(k, v);
	public inline function exists(k) return map.exists(k);
	public inline function remove(k) return map.remove(k);
	public inline function keys() return map.keys();
	public inline function iterator() return map.iterator();
	public inline function keyValueIterator() return map.keyValueIterator();
	public inline function toString() return map.toString();
	public inline function clear() return map.clear();
}

//@:transitive
@:multiType(@:followWithAbstracts K)
@:forward(clear)
abstract MultiMap<K, V>(BaseMultiMap<K, V>) {
	public var size(get, never): Int;
	inline function get_size() return this.size;

	public function new();

	public overload extern inline function has(key) return has_K(key);
	function has_K(key) {
		return this.exists(key) && this.get(key).length != 0;
	}

	public overload extern inline function has(key, value) return has_K_V(key, value);
	function has_K_V(key, value) {
		return this.exists(key) && this.get(key).contains(value);
	}

	public function add(key, value) {
		if(this.exists(key)) {
			this.get(key).push(value);
		} else {
			this.set(key, [value]);
		}
	}

	public inline function set(key, values) {
		this.set(key, values);
	}

	inline function rawGet(key) {
		return this.get(key);
	}

	public function get(key) {
		if(has(key)) {
			return this.get(key).copy();
		} else {
			throw new KeyError(key);
		}
	}

	public function getOne(key) {
		if(has(key)) switch this.get(key) {
			case [value]: return value;
			default: throw new KeyError(key);
		} else {
			throw new KeyError(key);
		}
	}

	public function find(key) {
		return if(has(key)) {
			Some(this.get(key).copy());
		} else {
			None;
		}
	}

	public overload extern inline function remove(key) return remove_K(key);
	inline function remove_K(key) {
		return this.remove(key);
	}

	public overload extern inline function remove(key, value) return remove_K_V(key, value);
	function remove_K_V(key, value) {
		if(this.exists(key)) {
			return this.get(key).remove(value);
		} else {
			throw new KeyError(key);
		}
	}

	public function sizeOf(key) {
		if(this.exists(key)) {
			return this.get(key).length;
		} else {
			return 0;
		}
	}

	public inline function keys() {
		return [for(key in this.keys()) key];
	}

	public inline function values() {
		return [for(values in this.iterator()) values.copy()];
	}

	public inline function allValues() {
		return [for(values in this.iterator()) for(value in values) value];
	}

	public inline function iterator() {
		return allValues().iterator();
	}
	
	public inline function keyValueIterator(): KeyValueIterator<K, V> {
		return cast [for(key => values in this.keyValueIterator()) for(value in values) {key: key, value: value}].iterator();
	}

	public inline function copy(): MultiMap<K, V> {
		return cast this.copy();
	}

	public inline function toString() {
		return this.toString();
	}

	@:to static inline function toIntMultiMap<K: Int, V>(m: BaseMultiMap<K, V>) return new IntMultiMap<V>();
	@:to static inline function toStringMultiMap<K: String, V>(m: BaseMultiMap<K, V>) return new StringMultiMap<V>();
	@:to static inline function toObjectMultiMap<K: {}, V>(m: BaseMultiMap<K, V>) return new ObjectMultiMap<K, V>();
	@:to static inline function toEnumValueMultiMap<K: EnumValue, V>(m: BaseMultiMap<K, V>) return new EnumValueMultiMap<K, V>();

	//@:from static inline function fromIntMultiMap<V>(m: IntMultiMap<V>): MultiMap<Int, V> return cast m;
	//@:from static inline function fromStringMultiMap<V>(m: StringMultiMap<V>): MultiMap<String, V> return cast m;
	//@:from static inline function fromObjectMultiMap<K: {}, V>(m: ObjectMultiMap<K, V>): MultiMap<K, V> return cast m;
	//@:from static inline function fromEnumValueMultiMap<K: EnumValue, V>(m: EnumValueMultiMap<K, V>): MultiMap<K, V> return cast m;
}