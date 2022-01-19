type T
alias ISet[T] = Immutable.Set[T]

class Cache {
	my types is getter = ISet[Type] #[]
	my cats is getter = ISet[Category] #[]
	my files is getter = ISet[File] #[]
	my dirs is getter = ISet[Dir] #[]

	on [has: type (Type)] (Bool) => return types[contains: type]
	on [has: cat (Category)] (Bool) => return cats[contains: cat]
	on [has: file (File)] (Bool) => return files[contains: file]
	on [has: dir (Dir)] (Bool) => return dirs[contains: dir]

	operator `+` [decl (TypeDecl)] => return this + decl.thisType
	operator `+` [type (Type)] (Cache) => return Cache[types: types[add: type] :cats :files :dirs]
	operator `+` [cat (Category)] (Cache) => return Cache[:types cats: cats[add: cat] :files :dirs]
	operator `+` [file (File)] (Cache) => return Cache[:types :cats files: files[add: file] :dirs]
	operator `+` [dir (Dir)] (Cache) => return Cache[:types :cats :files dirs: dirs[add: dir]]

	operator `-` [type (Type)] (Cache) => return Cache[types: types[remove: type] :cats :files :dirs]
	operator `-` [cat (Category)] (Cache) => return Cache[:types cats: cats[remove: cat] :files :dirs]
	operator `-` [file (File)] (Cache) => return Cache[:types :cats files: files[remove: file] :dirs]
	operator `-` [dir (Dir)] (Cache) => return Cache[:types :cats :files dirs: dirs[remove: dir]]

	operator `?` (Bool) => return types? || cats? || files? || dirs?
}

alias TypeCache (ISet[Type]) {
	on [has: type (Type)] (Bool) => return this[contains: type]
	
	operator `+` [type (Type)] (TypeCache) => return this[add: type]

	operator `-` [type (Type)] (TypeCache) => return this[remove: type]
}