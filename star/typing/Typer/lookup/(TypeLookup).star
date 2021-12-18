protocol TypeLookup of HasErrors {
	on [makeTypePath: path (TypePath)] (Type)
	
	on [
		findType: path (LookupPath)
		search: (Search)
		from: (Maybe[AnyTypeDecl])
		depth: (Int) = 0
		cache: (Cache) = Cache[new]
	] (Maybe[Type])

	on [
		in: ctx (Ctx)
		findCategory: cat (Type)
		forType: (Type)
		from: (AnyTypeDecl)
		cache: (Cache) = Cache[new]
	] (Array[Category])
}


category TypeLookup for Array[Type] {
	on [resolveArgs: from (AnyTypeDecl), errors (Array[Diagnostic])] (Array[Type]) {
		return this[collect: {|arg|
			match arg at Type[depth: my depth lookup: my lookup source: source] {
				match source[findType: lookup search: Search.start :from :depth] at Maybe[the: my type] {
					return type
				} else {
					errors[add: Error[invalidTypeLookup: lookup.span because: "Unknown type \(arg.simpleName)"]]
					return arg
				}
			} else {
				return arg
			}
		}]
	}
}