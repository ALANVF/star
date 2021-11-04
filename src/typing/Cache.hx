package typing;



@:forward
@:notNull
abstract Cache(List<{}>) from List<{}> to List<{}> {
	public static final empty: Cache = Nil;

	public inline function new() this = Nil;

	@:op(A + B)
	inline function add<T: {}>(type: T): Cache {
		return Cons(type, this);
	}

	/*@:op(A - B)
	inline function remove(type: Type): TypeCache {
		return this.;
	}*/

	public var list(get, never): List<{}>; inline function get_list() return this;
}