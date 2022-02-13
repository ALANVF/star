package util;

import haxe.macro.Context;
import haxe.macro.Expr;
import haxe.Constraints.IMap;
import haxe.ds.HashMap;
import haxe.ds.BalancedTree.TreeNode;
import haxe.ds.EnumValueMap;
import haxe.ds.IntMap;
import haxe.ds.StringMap;
import haxe.ds.ObjectMap;

#if !macro
class IntMaps {
	@:hlNative("std", "hisize")
	static function hisize(m: hl.types.IntMap): Int {
		return 0;
	}

	public static inline function size<V>(map: IntMap<V>) {
		return hisize(@:privateAccess map.h);
	}

	public static inline function forEach<V>(map: IntMap<V>, fn: (Int, V) -> Void) {
	#if hl
		final imap = @:privateAccess map.h;
		final ikeys = imap.keysArray();
		final ivals = imap.valuesArray();
		final isize = ikeys.length;
		for(i in 0...isize)
	#else
		for(k => v in map)
	#end
		{
		#if hl
			final k = ikeys[i];
			final v: V = ivals[i];
		#end

			fn(k, v);
		}
	}
}

class StringMaps {
	@:hlNative("std", "hbsize")
	static function hbsize(m: hl.types.BytesMap): Int {
		return 0;
	}

	public static inline function size<V>(map: StringMap<V>) {
		return hbsize(@:privateAccess map.h);
	}

	public static inline function forEach<V>(map: StringMap<V>, fn: (String, V) -> Void) {
	#if hl
		final imap = @:privateAccess map.h;
		final ikeys = imap.keysArray();
		final ivals = imap.valuesArray();
		final isize = ikeys.length;
		for(i in 0...isize)
	#else
		for(k => v in map)
	#end
		{
		#if hl
			final k = ikeys[i];
			final v: V = ivals[i];
		#end

			fn(@:privateAccess String.fromUCS2(k), v);
		}
	}
}

class ObjectMaps {
	@:hlNative("std", "hosize")
	static function hosize(m: hl.types.ObjectMap): Int {
		return 0;
	}

	public static inline function size<K: {}, V>(map: ObjectMap<K, V>) {
		return hosize(@:privateAccess map.h);
	}

	public static inline function forEach<K: {}, V>(map: ObjectMap<K, V>, fn: (K, V) -> Void) {
	#if hl
		final imap = @:privateAccess map.h;
		final ikeys = imap.keysArray();
		final ivals = imap.valuesArray();
		final isize = ikeys.length;
		for(i in 0...isize)
	#else
		for(k => v in map)
	#end
		{
		#if hl
			final k: K = ikeys[i];
			final v: V = ivals[i];
		#end

			fn(k, v);
		}
	}
}

class EnumValueMaps {
	@:noUsing
	static function sizeOfNode<K, V>(node: TreeNode<K, V>) {
		if(node == null) {
			return 0;
		} else {
			return sizeOfNode(node.left) + sizeOfNode(node.right) + 1;
		}
	}

	public static function size<K: EnumValue, V>(map: EnumValueMap<K, V>) {
		return sizeOfNode(@:privateAccess map.root);
	}

	/*@:noUsing
	static function _forEach<K, V>(node: TreeNode<K, V>) {

	}

	public static inline function forEach<K: EnumValue, V>(map: EnumValueMap<K, V>, fn: (K, V) -> Void) {
		@:privateAccess map.root
	}*/
}

class HashMaps {
	public static function size<K: {function hashCode(): Int;}, V>(map: HashMap<K, V>) {
		return @:privateAccess IntMaps.size(((untyped map).keys : IntMap<K>));
	}
}
#end

/*class _Maps {
	public static inline macro function size<K, V>(map: ExprOf<Map<K, V>>): ExprOf<Int> {
		return macro 0;
	}
}*/

class Maps {
	public static function pairs<K, V>(map: IMap<K, V>) {
		return [for(pair in map.keyValueIterator()) pair];
	}

	public static macro function size<K, V>(map: ExprOf<Map<K, V>>): ExprOf<Int> {
		final tmap = Context.typeof(map);
		final ftmap = Context.followWithAbstracts(tmap);

		switch ftmap {
			case TInst(_.get() => t, p): return switch t.name {
				case "IntMap": macro util.Maps.IntMaps.size($map);
				case "StringMap": macro util.Maps.StringMaps.size($map);
				case "ObjectMap": macro util.Maps.ObjectMaps.size($map);
				case "EnumValueMap": macro util.Maps.EnumValueMaps.size($map);
				case "HashMap": macro util.Maps.HashMaps.size($map);
				default: throw "error!";
			}
			default: throw "error!";
		}
	}
}