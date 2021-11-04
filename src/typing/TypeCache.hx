package typing;

@:forward
@:notNull
abstract TypeCache(List<Type>) from List<Type> to List<Type> {
	public inline function new() this = Nil;

	@:op(A + B)
	inline function add(type: Type): TypeCache {
		return Cons(type, this);
	}

	/*@:op(A - B)
	inline function remove(type: Type): TypeCache {
		return this.;
	}*/

	public var list(get, never): List<Type>; inline function get_list() return this;
}