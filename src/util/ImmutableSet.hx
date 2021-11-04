package util;

@:using(util.ImmutableSet)
private enum _ISet<T> {
	Entry(value: T, rest: _ISet<T>);
	End;
}


@:generic
@:publicFields
final class ISetIter<T> {
	private var iset: _ISet<T>;

	inline function new(iset: _ISet<T>) {
		this.iset = iset;
	}

	inline function hasNext(): Bool {
		return !iset.match(End);
	}

	inline function next(): T {
		switch iset {
			case Entry(v, rest):
				iset = rest;
				return v;
			
			case End:
				return @:nullSafety(Off) null;
		}
	}
}


@:generic
abstract BaseISet<T>(_ISet<T>) from _ISet<T> {
	public inline function exists(value: T): Bool throw "bad";

	@:op([])
	public inline function contains(value: T): Bool throw "bad";

	@:op(A + B)
	public inline function add(value: T): ImmutableSet<T> throw "bad";

	@:op(A - B)
	public inline function remove(value: T): ImmutableSet<T> throw "bad";
}


private function _enum_contains<T: EnumValue>(iset: _ISet<T>, value: T) return Util._match(iset,
	at(Entry(v, rest)) =>
		if(v.equals(value)) true
		else _enum_contains(rest, value),
	at(End) => false
);

private function _enum_add<T: EnumValue>(iset: _ISet<T>, value: T): _ISet<T> return Util._match(iset,
	at(Entry(v, rest)) =>
		if(v.equals(value)) iset
		else Entry(v, _enum_add(rest, value)),
	at(End) => Entry(value, End)
);

private function _enum_remove<T: EnumValue>(iset: _ISet<T>, value: T): _ISet<T> return Util._match(iset,
	at(Entry(v, rest)) =>
		if(v.equals(value)) rest
		else Entry(v, _enum_remove(rest, value)),
	at(End) => End
);

@:generic
abstract _ImmutableEnumSet<T: EnumValue>(_ISet<T>) from _ISet<T> {
	public inline function new() this = End;

	private var repr(get, never): _ISet<T>; private inline function get_repr() return this;
	private var self(get, never): _ImmutableEnumSet<T>; private inline function get_self(): _ImmutableEnumSet<T> return this;


	@:op([])
	public function contains(value: T): Bool {
		return inline _enum_contains(this, value);
	}

	public function containsAny(other: _ImmutableEnumSet<T>): Bool {
		for(value in other) {
			if(contains(value)) {
				return true;
			}
		}

		return false;
	}

	@:op(A + B)
	public function add(value: T): _ImmutableEnumSet<T> {
		return inline _enum_add(this, value);
	}

	@:op(A - B)
	public function remove(value: T): _ImmutableEnumSet<T> {
		return inline _enum_remove(this, value);
	}

	
	@:op(A | B)
	public function union(other: _ImmutableEnumSet<T>): _ImmutableEnumSet<T> {
		if(this.match(End)) return other;
		else if(other.repr.match(End)) return self;
		else {
			var res = self;

			for(value in other) {
				res = res.add(value);
			}

			return res;
		}
	}



	public inline function iterator(): Iterator<T> {
		return new ISetIter<T>(this);
	}
}


//@:generic
private function _size<T>(iset: _ISet<T>, res: Int): Int return Util._match(iset,
	at(Entry(_, rest)) => _size(rest, res + 1),
	at(End) => 0
);


@:multiType(@:followWithAbstracts T)
@:generic
abstract ImmutableSet<T>(BaseISet<T>) from _ISet<T> {
	public function new();

	public var size(get, never): Int; inline function get_size() {
		return _size(cast this, 0);
	}

	@:op([])
	public inline function contains(value: T): Bool return this.contains(value);

	@:op(A + B)
	public inline function add(value: T): ImmutableSet<T> return this.add(value);

	@:op(A - B)
	public inline function remove(value: T): ImmutableSet<T> return this.remove(value);

	
	@:to public static inline function toEnumISet<T: EnumValue>(m: BaseISet<T>): ImmutableEnumSet<T> return new ImmutableEnumSet<T>();
	//@:to static inline function toDefaultISet<T>(m: _ISet<T>): DefaultISet<K,V> return new DefaultISet<K,V>();
	//@:to static inline function toBaseISet<T>(m: _ISet<T>): BaseISet<K,V> return new BaseISet<K,V>();
}