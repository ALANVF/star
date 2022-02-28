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
	on [resolveArgs: from (Maybe[AnyTypeDecl]), errors (Array[Error])] (Array[Type]) {
		return this[collect: {|arg (Type)|
			match arg at Type[depth: my depth lookup: my lookup source: my source] {
				match source[findType: lookup search: Search.start :from :depth] at Maybe[the: my type] {
					return type
				} else {
					errors[add: TypeError[invalidTypeLookup: lookup.span why: "Unknown type \(arg.simpleName)"]]
					return arg
				}
			} else {
				return arg
			}
		}]
	}
}