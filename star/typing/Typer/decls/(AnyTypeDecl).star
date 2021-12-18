protocol AnyTypeDecl of Typeable, Decl {
	my lookup (TypeLookup) is getter
	my thisType (Type) is getter


	;== Categories

	on [
		in: (Ctx)
		findThisCategory: cat (Type)
		from: (AnyTypeDecl)
		cache: (Cache)
	] (Array[Category]) {
		return this[:in findCategory: cat forType: thisType :from :cache]
	}
}