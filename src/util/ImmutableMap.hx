package util;

@:using(util.ImmutableMap)
private enum _IMap<K, V> {
	Pair(key: K, value: V, rest: _IMap<K, V>);
	End;
}


@:generic
@:publicFields
final class IMapKVIter<K, V> {
	private var imap: _IMap<K, V>;

	inline function new(imap: _IMap<K, V>) {
		this.imap = imap;
	}

	inline function hasNext(): Bool {
		return !imap.match(End);
	}

	inline function next(): {key: K, value: V} {
		switch imap {
			case Pair(k, v, rest):
				imap = rest;
				return {key: k, value: v};
			
			case End:
				return @:nullSafety(Off) null;
		}
	}
}


@:generic
abstract BaseIMap<K, V>(_IMap<K, V>) from _IMap<K, V> {
	public inline function exists(key: K): Bool return _default_exists(this, key);

	@:op([])
	public function get(key: K): Null<V> return inline _default_get(this, key);

	public function set(key: K, value: V): ImmutableMap<K, V> return inline _default_set(this, key, value);

	@:op([])
	inline function assign(key: K, value: V) {
		this = (cast set(key, value):_IMap<K,V>);
	}


	public inline function keyValueIterator(): KeyValueIterator<K, V> {
		return new IMapKVIter<K, V>(this);
	}

	public function toString() {
		return "{" + _default_toString(this) + "}";
	}
}


private function _enum_exists<K: EnumValue, V>(imap: _IMap<K, V>, key: K) return Util._match(imap,
	at(Pair(k, _, rest)) =>
		if(k.equals(key)) true
		else _enum_exists(rest, key),
	at(End) => false
);

private function _enum_get<K: EnumValue, V>(imap: _IMap<K, V>, key: K): Null<V> return Util._match(imap,
	at(Pair(k, v, rest)) =>
		if(k.equals(key)) v
		else _enum_get(rest, key),
	at(End) => null
);

private function _enum_set<K: EnumValue, V>(imap: _IMap<K, V>, key: K, value: V): _IMap<K, V> return Util._match(imap,
	at(Pair(k, v, rest)) =>
		if(k.equals(key)) Pair(k, value, rest)
		else Pair(k, v, _enum_set(rest, key, value)),
	at(End) => Pair(key, value, End)
);

private function _enum_toString<K: EnumValue, V>(imap: _IMap<K, V>): String return Util._match(imap,
	at(Pair(k, v, End)) => '$k => $v',
	at(Pair(k, v, rest)) => '$k => $v, '+_enum_toString(rest),
	at(End) => ""
);

@:generic
abstract _ImmutableEnumMap<K: EnumValue, V>(_IMap<K, V>) from _IMap<K, V> {
	public inline function new() this = End;

	public function exists(key: K): Bool {
		return _enum_exists(this, key);
	}

	@:op([])
	function get(key: K): Null<V> {
		return inline _enum_get(this, key);
	}

	public function set(key: K, value: V): _ImmutableEnumMap<K, V> {
		return inline _enum_set(this, key, value);
	}

	@:op([])
	inline function assign(key: K, value: V) {
		this = (cast set(key, value):_IMap<K,V>);
	}


	public inline function keyValueIterator(): KeyValueIterator<K, V> {
		return new IMapKVIter<K, V>(this);
	}


	public function toString() {
		return "{" + _enum_toString(this) + "}";
	}
}


//@:generic
private function _size<K, V>(imap: _IMap<K, V>, res: Int): Int return Util._match(imap,
	at(Pair(_, _, rest)) => _size(rest, res + 1),
	at(End) => 0
);


private function _default_exists<K, V>(imap: _IMap<K, V>, key: K) return Util._match(imap,
	at(Pair(k, _, rest)) =>
		if(k == key) true
		else _default_exists(rest, key),
	at(End) => false
);


private function _default_get<K, V>(imap: _IMap<K, V>, key: K): Null<V> return Util._match(imap,
	at(Pair(k, v, rest)) =>
		if(k == key) v
		else _default_get(rest, key),
	at(End) => null
);


private function _default_set<K, V>(imap: _IMap<K, V>, key: K, value: V): _IMap<K, V> return Util._match(imap,
	at(Pair(k, v, rest)) =>
		if(k == key) Pair(k, value, rest)
		else Pair(k, v, _default_set(rest, key, value)),
	at(End) => Pair(key, value, End)
);


private function _default_toString<K, V>(imap: _IMap<K, V>): String return Util._match(imap,
	at(Pair(k, v, End)) => '$k => $v',
	at(Pair(k, v, rest)) => '$k => $v, '+_default_toString(rest),
	at(End) => ""
);


//@:multiType(@:followWithAbstracts K)
@:generic
abstract ImmutableMap<K, V>(BaseIMap<K, V>) from _IMap<K, V> {
	public function new() this = End;

	public var size(get, never): Int; inline function get_size() {
		return _size(cast this, 0);
	}

	public inline function exists(key: K): Bool return this.exists(key);

	@:op([])
	inline function get(key: K): Null<V> return this.get(key);

	public inline function set(key: K, value: V): ImmutableMap<K, V> return this.set(key,value);

	@:op([])
	inline function assign(key: K, value: V) this[key] = value;

	
	public inline function keyValueIterator() return this.keyValueIterator();

	public inline function toString() return this.toString();

	
	//@:to public static inline function toEnumIMap<K: EnumValue, V>(m: BaseIMap<K, V>): ImmutableEnumMap<K,V> return new ImmutableEnumMap<K,V>();
	//@:to static inline function toDefaultIMap<K, V>(m: _IMap<K, V>): DefaultIMap<K,V> return new DefaultIMap<K,V>();
	//@:to static inline function toBaseIMap<K, V>(m: _IMap<K, V>): BaseIMap<K,V> return new BaseIMap<K,V>();
}