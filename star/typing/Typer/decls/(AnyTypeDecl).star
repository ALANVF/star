protocol AnyTypeDecl of Typeable, TypeLookupDecl {
	my lookup (TypeLookup) is getter
	my thisType (Type) is getter


	on [name] (Str) is getter


	;== Categories

	on [
		in: (Ctx)
		findThisCategory: cat (Type)
		from: (Type)
		cache: (Cache)
	] (Array[Category]) {
		return this[:in findCategory: cat forType: thisType :from :cache]
	}
}