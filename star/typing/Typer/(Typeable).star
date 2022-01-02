protocol Typeable of TypeLookup {
	;== Display info
	
	on [fullName] (Str) is getter => return this[fullName: TypeCache[new]]
	on [fullName: cache (TypeCache)] (Str)


	;== Type checking

	on [hasParent: decl (TypeDecl)] (Bool)
	on [hasParent: type (Type)] (Bool)

	on [hasChild: decl (TypeDecl)] (Bool)
	on [hasChild: type (Type)] (Bool)

	on [hasStrictChild: type (Type)] (Bool)

	on [hasRefinement: decl (TypeDecl)] (Bool)
	on [hasRefinement: type (Type)] (Bool)


	;== Unification

	on [strictUnifyWith: type (Type)] (Maybe[Type])


	;== Generics

	on [acceptsArgs: args (Array[Type])] (Bool)

	on [applyArgs: args (Array[Type])] (Maybe[Type])

	
	;== Attributes

	on [isNative: native (Native)] (Bool)

	on [isFlags] (Bool)

	on [isStrong] (Bool)

	on [isUncounted] (Bool)


	;== Effects tracking

	on [trackEffectsIn: ctx (Ctx)] (Maybe[Effects])

	on [applyArgs: args (Array[Type]) trackEffectsIn: ctx (Ctx)] (Maybe[Tuple[Type, Effects]])


	;== Privacy

	on [canSee: member (Member)] (Bool)
	on [canSee: method (RealMethod)] (Bool)


	;== Members

	on [instanceMembers: from (AnyTypeDecl)] (Array[Member])

	
	;== Method lookup

	on [
		in: ctx (Ctx)
		findStatic: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool) = false
		cache: (TypeCache) = TypeCache[new]
	] (Maybe[SingleStatic])

	on [
		in: ctx (Ctx)
		findStatic: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool) = false
		cache: (TypeCache) = TypeCache[new]
	] (MultiStatics)

	on [
		in: ctx (Ctx)
		findInstance: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool) = false
		cache: (TypeCache) = TypeCache[new]
	] (Maybe[SingleInst])

	on [
		in: ctx (Ctx)
		findInstance: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool) = false
		cache: (TypeCache) = TypeCache[new]
	] (MultiInsts)

	on [
		in: ctx (Ctx)
		findCast: target (Type)
		from: (AnyTypeDecl)
		cache: (TypeCache) = TypeCache[new]
	] (Casts)

	on [
		in: ctx (Ctx)
		findUnaryOp: op (UnaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache) = TypeCache[new]
	] (Maybe[UnaryOp])

	on [
		in: ctx (Ctx)
		findBinaryOp: op (BinaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache) = TypeCache[new]
	] (BinaryOps)


	;== Categories

	on [
		in: ctx (Ctx)
		findThisCategory: cat (Type)
		from: (AnyTypeDecl)
		cache: (Cache) = Cache[new]
	] (Array[Category])
}