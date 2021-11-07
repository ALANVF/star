package typing;

interface ITypeable extends ITypeLookup {
	// Display info
	
	function fullName(cache: TypeCache = Nil): String;

	
	// Type checking

	function hasParentDecl(decl: TypeDecl): Bool;
	function hasParentType(type: Type): Bool;

	function hasChildDecl(decl: TypeDecl): Bool;
	function hasChildType(type: Type): Bool;

	function hasStrictChildType(type: Type): Bool;

	function hasRefinementDecl(decl: TypeDecl): Bool;
	function hasRefinementType(type: Type): Bool;


	// Unification

	function strictUnifyWithType(type: Type): Null<Type>;


	// Generics

	function acceptsArgs(args: Array<Type>): Bool;

	function applyArgs(args: Array<Type>): Null<Type>;


	// Attributes

	function isNative(kind: NativeKind): Bool;

	function isFlags(): Bool;

	function isStrong(): Bool;

	function isUncounted(): Bool;


	// Effects tracking

	function trackEffectsIn(ctx: Ctx): Null<Effects>;

	function applyArgsTrackEffects(args: Array<Type>, ctx: Ctx): Null<Tuple2<Type, Effects>>;


	// Privacy

	function canSeeMember(member: Member): Bool;

	function canSeeMethod(method: AnyMethod): Bool;


	// Members

	function instMembers(from: AnyTypeDecl): Array<Member>;


	// Method lookup
	
	function findSingleStatic(
		ctx: Ctx,
		name: String,
		from: AnyTypeDecl,
		getter: Bool = false,
		cache: TypeCache = Nil
	): Null<SingleStaticKind>;

	function findMultiStatic(
		ctx: Ctx,
		names: Array<String>,
		from: AnyTypeDecl,
		setter: Bool = false,
		cache: TypeCache = Nil
	): Array<MultiStaticKind>;

	function findSingleInst(
		ctx: Ctx,
		name: String,
		from: AnyTypeDecl,
		getter: Bool = false,
		cache: TypeCache = Nil
	): Null<SingleInstKind>;

	function findMultiInst(
		ctx: Ctx,
		names: Array<String>,
		from: AnyTypeDecl,
		setter: Bool = false,
		cache: TypeCache = Nil
	): Array<MultiInstKind>;

	function findCast(
		ctx: Ctx,
		target: Type,
		from: AnyTypeDecl,
		cache: TypeCache = Nil
	): Array<CastKind>;

	function findUnaryOp(
		ctx: Ctx,
		op: UnaryOp,
		from: AnyTypeDecl,
		cache: TypeCache = Nil
	): Null<UnaryOpKind>;

	function findBinaryOp(
		ctx: Ctx,
		op: BinaryOp,
		from: AnyTypeDecl,
		cache: TypeCache = Nil
	): Array<BinaryOpKind>;


	// Categories

	function findThisCategory(
		ctx: Ctx,
		cat: Type,
		from: AnyTypeDecl,
		cache: Cache = Nil
	): Array<Category>;
}