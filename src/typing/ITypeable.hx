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
	function getNative(): Null<NativeKind>;

	function isFlags(): Bool;

	function isStrong(): Bool;

	function isUncounted(): Bool;


	// Iterating

	function iterElemType(): Null<Type>;

	function iterAssocType(): Null<Tuple2<Type, Type>>;


	// Effects tracking

	function trackEffectsIn(ctx: Ctx): Null<Effects>;

	function applyArgsTrackEffects(args: Array<Type>, ctx: Ctx): Null<Tuple2<Type, Effects>>;


	// Privacy

	function canSeeMember(member: Member): Bool;

	function canSeeMethod(method: AnyMethod): Bool;


	// Cases

	function allValueCases(): Array<ValueCase>;
	
	function allTaggedCases(): Array<TaggedCase>;



	// Members

	function instMembers(from: AnyTypeDecl): Array<Member>;

	function findInstMember(
		ctx: Ctx,
		name: String,
		allowStatic: Bool = true,
		onlyParents: Bool = false
	): Null<MemberKind>;


	// Method lookup
	
	function findSingleStatic(
		ctx: Ctx,
		name: String,
		from: Type,
		getter: Bool = false,
		cache: TypeCache = Nil
	): Null<SingleStaticKind>;

	function findMultiStatic(
		ctx: Ctx,
		names: Array<String>,
		from: Type,
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
		from: Type,
		setter: Bool = false,
		cache: TypeCache = Nil
	): Array<MultiInstKind>;

	function findCast(
		ctx: Ctx,
		target: Type,
		from: Type,
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
		from: Type,
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