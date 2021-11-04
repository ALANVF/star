package typing;

interface ITypeLookup {
	function makeTypePath(path: TypePath): Type;
	
	function findType(
		path: LookupPath,
		search: Search,
		from: Null<AnyTypeDecl>,
		depth: Int = 0,
		cache: Cache = Nil
	): Null<Type>;

	function findCategory(
		ctx: Ctx,
		cat: Type,
		forType: Type,
		from: AnyTypeDecl,
		cache: Cache = Nil
	): Array<Category>;
}