kind Type of TypeLookup, Typeable {
	;-- An unevaluated type path
	has [depth: (Int) lookup: (LookupPath) source: (TypeLookup)]

	;-- An indirect type path lookup
	has [type: (Type) lookup: (LookupPath)]

	;-- A concrete type decl
	has [decl: (TypeDecl)]

	;-- A concrete, generic type decl applied with expanded type parameters
	has [decl: (TypeDecl) params: (Array[Type]) ctx: (TypeVarCtx)]

	;-- A `This` type
	has [this: source (AnyTypeDecl)]

	;-- A `_` type
	has [blank: span (Span)] => [blank] {
		_.span = Maybe[the: span]
	}

	;-- Multiple generic candidates
	has [types: (Array[Type])]

	;-- A type applied with type arguments
	has [type: (Type) args: (Array[Type])]

	;-- A type variable
	has [typevar: (TypeVar)]

	;-- A modular type
	has [type: (Type) unit: (Unit)]
	
	;-- Optional location of the type
	my span (Maybe[Span]) is getter = Maybe[none]

	on [span: span' (Span)] is setter => span = Maybe[the: span']


	on [simplify: ctx (Ctx)] (Type) {
		match this {
			at This[depth: my depth lookup: my lookup source: my source] {
				match source[
					findType: lookup
					search: Search.start
					from: Maybe[the: ctx.typeDecl]
					:depth
				] at Maybe[the: my type] {
					return type
				} else {
					throw "Type `\(this.fullName)` does not exist!"
				}
			}

			at This[type: my type lookup: my lookup] {
				match type[
					findType: lookup
					search: Search.inside
					from: Maybe[the: ctx.typeDecl]
				] at Maybe[the: my type'] {
					return type'
				} else {
					throw "Type `\(this.fullName)` does not exist!"
				}
			}

			else {
				return this
			}
		}
	}


	;== Type lookup

	on [makeTypePath: path (TypePath)] (Type)

	on [
		findType: path (LookupPath)
		search: (Search)
		from: (Maybe[AnyTypeDecl])
		depth: (Int)
		cache: (Cache)
	] (Maybe[Type])


	;== Type resolution

	on [maybeIn: ctx (Ctx)] (Maybe[Type]) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify: ctx][maybeIn: ctx]
			}

			at This[decl: _] || This[decl: _ params: _ ctx: _] || This[this: _] || This[blank] {
				return Maybe[the: this]
			}

			at This[types: my types] {
				my types' = #[]

				for my type in: types {
					match type[maybeIn: ctx] at Maybe[the: my type'] {
						types'[add: type']
					}
				}

				match types' {
					at #[]        => return Maybe[none]
					at #[my type] => return Maybe[the: type]
					else          => return Maybe[the: Type[:span types: types']]
				}
			}

			at This[type: This[types: my types] args: my args] {
				my types' = #[]
				my args' = args[collect: $0[in: ctx]]

				for my type in: types {
					match type[maybeIn: ctx] at Maybe[the: my type'] {
						match type'[applyArgs: args'] at Maybe[the: my type''] {
							types'[add: type'']
						}
					}
				}

				match types' {
					at #[]        => return Maybe[none]
					at #[my type] => return Maybe[the: type]
					else          => return Maybe[the: Type[:span types: types']]
				}
			}

			at This[type: my type args: my args] {
				match type[maybeIn: ctx] at Maybe[the: my type'] {
					return type'[applyArgs: args[collect: $0[in: ctx]]]
				} else {
					return Maybe[none]
				}
			}

			at This[typevar: my typevar] {
				match ctx[findTypevar: typevar] at Maybe[the: my type] {
					return Maybe[the: type]
				} else {
					return Maybe[the: this]
				}
			}

			at This[type: my type unit: _] {
				return type[maybeIn: ctx]
			}
		}
	}

	on [in: ctx (Ctx)] (Type) {
		match this[maybeIn: ctx] at Maybe[the: my type] {
			return type
		} else {
			throw "Error: invalid type `\(this[fullName])`!"
		}
	}


	;== Display info
	
	on [fullName] (Str) is getter => return this[fullName: TypeCache[new]]
	on [fullName: cache (TypeCache)] (Str)


	;== Type checking

	on [hasParent: decl (TypeDecl)] (Bool)
	on [hasParent: type (Type)] (Bool)

	on [hasChild: decl (TypeDecl)] (Bool)
	on [hasChild: type (Type)] (Bool)

	on [hasStrictChild: type (Type)] (Bool)



	;== Unification

	on [strictUnifyWith: type (Type)] (Maybe[Type])


	;== Generics

	on [acceptsArgs: args (Array[Type])] (Bool)

	on [applyArgs: args (Array[Type])] (Maybe[Type])


	;== Binding

	on [bindTo: onto (Type) in: ctx (TypeVarCtx)] (Type) {
		
	}

	
	;== Attributes

	on [isNative: native (Native)] (Bool)

	on [isFlags] (Bool)

	on [isStrong] (Bool)

	on [isUncounted] (Bool)


	;== Effects tracking

	on [trackEffectsIn: ctx (Ctx)] (Maybe[Effects]) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify: ctx][trackEffectsIn: ctx]
			}

			at This[decl: _] || This[decl: _ params: _ ctx: _] || This[this: _] || This[blank] {
				return Maybe[the: Effects.empty]
			}

			at This[types: my types] {
				;@@ TODO
				return Maybe[the: Effects.empty]
			}

			at This[type: my type args: my args] {
				match type[applyArgs: args trackEffectsIn: ctx] at Maybe[the: #{_, my effects}] {
					return Maybe[the: effects]
				} else {
					return Maybe[none]
				}
			}

			at This[typevar: my typevar] {
				;@@TODO
				return Maybe[the: Effects.empty]
			}

			at This[type: my type unit: _] {
				return type[trackEffectsIn: ctx]
			}
		}
	}

	on [applyArgs: args (Array[Type]) trackEffectsIn: ctx (Ctx)] (Maybe[Tuple[Type, Effects]]) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify: ctx][applyArgs: args trackEffectsIn: ctx]
			}

			at This[decl: my decl] {
				return decl[applyArgs: args trackEffectsIn: ctx]
			}

			;-- This theoretically shouldn't happen unless the type is curried
			at This[decl: _ params: _ ctx: _] {
				throw "bad"
			}

			at This[this: my source] {
				my typeDecl = ctx.typeDecl

				if source ?= typeDecl {
					return source.thisType[applyArgs: args trackEffectsIn: ctx]
				} else {
					;@@ TODO: how the hell do I make this work
					throw "Not yet implemented!"
				}
			}

			at This[blank] {
				return Maybe[the: #{Type[type: this :args], Effects.empty}]
			}

			at This[types: my types] {
				;@@ TODO
				return Maybe[the: #{Type[type: this :args], Effects.empty}]
			}

			;-- This theoretically shouldn't happen unless the type is curried
			at This[type: my type args: my args'] {
				throw "bad"
			}

			at This[typevar: my typevar] {
				;@@TODO
				return Maybe[the: #{Type[type: this :args], Effects.empty}]
			}

			at This[type: my type unit: _] {
				return type[applyArgs: args trackEffectsIn: ctx]
			}
		}
	}


	;== Privacy

	on [canSee: member (Member)] (Bool)
	on [canSee: method (RealMethod)] (Bool)


	;== Members

	on [instanceMembers] (Array[Member])

	
	;== Method lookup

	on [
		in: ctx (Ctx)
		findStatic: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleStatic])

	on [
		in: ctx (Ctx)
		findStatic: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiStatics)

	on [
		in: ctx (Ctx)
		findInstance: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleInst])

	on [
		in: ctx (Ctx)
		findInstance: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiInsts)

	on [
		in: ctx (Ctx)
		findCast: target (Type)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (Casts)

	on [
		in: ctx (Ctx)
		findUnaryOp: op (UnaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (Maybe[UnaryOp])

	on [
		in: ctx (Ctx)
		findBinaryOp: op (BinaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (BinaryOps)


	;== Categories
	
	on [
		findCategory: cat (Type)
		forType: (Type)
		from: (AnyTypeDecl)
		cache: (Cache)
	] (Array[Category])

	on [
		findThisCategory: cat (Type)
		from: (AnyTypeDecl)
		cache: (Cache)
	] (Array[Category])
}


category Type for TypeLookup {
	on [fullPath] (Maybe[Str]) {
		match this {
			at my file (File) {
				match file.dir at my unit (Unit) {
					my path = unit.name[new]

					while true {
						match unit.outer at unit = (Unit) {
							path
							-> [prepend: #"."]
							-> [prepend: unit.name]
						} else {
							return Maybe[the: path]
						}
					}
				} else {
					return Maybe[none]
				}
			}

			at my decl (TypeDecl) {
				match decl.lookup[Type fullPath] at Maybe[the: my path] {
					return Maybe[the: "\(path).\(decl.name)"]
				} else {
					return Maybe[the: decl.name[new]]
				}

			}

			else => return Maybe[the: "???"]
		}
	}
}