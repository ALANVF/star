package typing;

typedef ILookupType = {
	function makeTypePath(path: TypePath): Type;
	
	function findType(path: List<String>, ?absolute: Bool, ?cache: List<{}>): Option<Type>;
}