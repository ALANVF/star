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


	;== Printing

	on [simpleName] (Str) is getter

	on [fullName] (Str) is getter => return this[fullName: TypeCache #[]]
	on [fullName: cache (TypeCache)] (Str) is hidden


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
				return this[simplify][maybeIn: ctx]
			}

			at This[decl: _] || This[decl: _ params: _ ctx: _] || This[this: _] || This[blank] {
				return Maybe[the: this]
			}

			at This[types: my types] {
				my types' = Array[Type] #[]

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
				my types' = Array[Type] #[]
				my args' = args[collect: Type$0[in: ctx]]

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
					return type'[applyArgs: args[collect: Type$0[in: ctx]]]
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
			throw "Error: invalid type `\(this.fullName)`!"
		}
	}

	on [in: tctx (TypeVarCtx)] (Type) {
		my ctx = Ctx[typevars: tctx thisType: this]
		match this[maybeIn: ctx] at Maybe[the: my type] {
			return type
		} else {
			throw "Error: invalid type `\(this.fullName)`!"
		}
	}

	on [maybeFrom: type (Type)] (Maybe[Type]) {

	}

	on [from: type (Type)] (Type) {
		match this[maybeFrom: type] at Maybe[the: my type'] {
			return type'
		} else {
			throw "Error: invalid type `\(this.fullName) in \(type.fullName)`!"
		}
	}

	on [simplify] (Type) {
		match this {
			at This[depth: my depth lookup: my lookup source: my source] {
				match source[
					findType: lookup
					search: Search.start
					from: Maybe[the: source[AnyTypeDecl]]
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
					from: Maybe[none];Maybe[the: ctx.typeDecl]
				] at Maybe[the: my type'] {
					return type'
				} else {
					throw "Type `\(this.fullName)` does not exist!"
				}
			}

			at This[type: This[types: my types] args: my args] {
				my args' = args[collect: Type$0[simplify]]
				my types' = types[keepIf: Type$0[acceptsArgs: args']]

				if args'[none: Type$0[hasTypevars]] {
					types' = types'[Type mostSpecific]
				}

				match types' {
					at #[] => throw "bad"
					at #[This[decl: my decl]] {
						match decl[applyArgs: args'] at Maybe[the: my type'] {
							return type'
							-> span = span
						} else {
							throw "error!"
						}
					}
					at #[my type'] => return This[type: type' args: args' :span]
					else {
						if types'.length ?= types.length {
							return this
						} else {
							return This[type: This[types: types' :span] args: args' :span]
						}
					}
				}
			}

			at This[type: This[decl: my decl] args: my args] {
				my args' = args[collect: Type$0[simplify]]
				match decl[applyArgs: args'] at Maybe[the: my type'] {
					return type'
					-> span = span
				} else {
					throw "error!"
				}
			}

			at This[type: my type args: my args] {
				return This[
					type: type[simplify]
					args: args[collect: Type$0[simplify]]
				]
			}

			else {
				return this
			}
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

	on [unifyWith: type (Type)] (Maybe[Type])

	on [strictUnifyWith: type (Type)] (Maybe[Type])


	;== Generics

	on [acceptsArgs: args (Array[Type])] (Bool)

	on [applyArgs: args (Array[Type])] (Maybe[Type])

	on [hasTypevars] (Bool)


	;== Binding

	on [bindTo: onto (Type) in: ctx (TypeVarCtx)] (Type) {
		
	}

	
	;== Attributes

	on [isNative: (Native)] (Bool) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify][:isNative]
			}
			at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
				return decl[:isNative]
			}
			at This[this: my decl] => return decl[:isNative]
			at This[blank] => throw "bad"
			at This[types: my types] => return types[any: Type$0[:isNative]]
			at This[type: my type args: _] => return type[:isNative]
			at This[typevar: my typevar] => return typevar[:isNative]
			at This[type: my type unit: _] => return type[:isNative]
		}
	}

	on [isFlags] (Bool) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify][isFlags]
			}
			at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
				return decl[isFlags]
			}
			at This[this: my decl] => return decl[isFlags]
			at This[blank] => throw "bad"
			at This[types: my types] => return types[any: Type$0[isFlags]]
			at This[type: my type args: _] => return type[isFlags]
			at This[typevar: my typevar] => return typevar[isFlags]
			at This[type: my type unit: _] => return type[isFlags]
		}
	}

	on [isStrong] (Bool) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify][isStrong]
			}
			at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
				return decl[isStrong]
			}
			at This[this: my decl] => return decl[isStrong]
			at This[blank] => throw "bad"
			at This[types: my types] => return types[any: Type$0[isStrong]]
			at This[type: my type args: _] => return type[isStrong]
			at This[typevar: my typevar] => return typevar[isStrong]
			at This[type: my type unit: _] => return type[isStrong]
		}
	}

	on [isUncounted] (Bool) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify][isUncounted]
			}
			at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
				return decl[isUncounted]
			}
			at This[this: my decl] => return decl[isUncounted]
			at This[blank] => throw "bad"
			at This[types: my types] => return types[any: Type$0[isUncounted]]
			at This[type: my type args: _] => return type[isUncounted]
			at This[typevar: my typevar] => return typevar[isUncounted]
			at This[type: my type unit: _] => return type[isUncounted]
		}
	}


	;== Iterating

	on [iterElemType] (Maybe[Type]) {
		my result = {
			match this {
				at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
					return this[simplify][iterElemType]
				}

				at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
					return decl[iterElemType]
				}

				at This[this: my decl] => return decl[iterElemType]

				at This[blank] => throw "bad"
				
				at This[types: my types] {
					match types[Type leastSpecific] {
						at #[] => return Maybe[none]
						at #[my type] => return type[iterElemType]
						at my types' => throw "todo"
					}
				}

				at This[type: my type args: my args] {
					match type[applyArgs: args] at Maybe[the: my type'] {
						match type'[iterElemType] at Maybe[the: my elem] {
							return Maybe[the: elem[from: type']]
						} else {
							return Maybe[none]
						}
					} else {
						return Maybe[none]
					}
				}

				at This[typevar: my typevar] => return typevar[iterElemType]

				at This[type: my type unit: _] => return type[iterElemType]
			}
		}

		match result at Maybe[the: my elem] {
			return Maybe[the: elem[from: this]]
		} else {
			return Maybe[none]
		}
	}

	on [iterAssocType] (Maybe[Tuple[Type, Type]]) {
		my result = {
			match this {
				at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
					return this[simplify][iterAssocType]
				}

				at This[decl: my decl] || This[decl: my decl params: _ ctx: _] {
					return decl[iterAssocType]
				}

				at This[this: my decl] => return decl[iterAssocType]

				at This[blank] => throw "bad"
				
				at This[types: my types] {
					match types[Type leastSpecific] {
						at #[] => return Maybe[none]
						at #[my type] => return type[iterAssocType]
						at my types' => throw "todo"
					}
				}

				at This[type: my type args: my args] {
					match type[applyArgs: args] at Maybe[the: my type'] {
						match type'[iterAssocType] at Maybe[the: #{my k, my v}] {
							return Maybe[the: #{k[from: type'], v[from: type']}]
						} else {
							return Maybe[none]
						}
					} else {
						return Maybe[none]
					}
				}

				at This[typevar: my typevar] => return typevar[iterAssocType]

				at This[type: my type unit: _] => return type[iterAssocType]
			}
		}

		match result at Maybe[the: #{my k, my v}] {
			return Maybe[the: #{k[from: this], v[from: this]}]
		} else {
			return Maybe[none]
		}
	}


	;== Effects tracking

	on [trackEffectsIn: ctx (Ctx)] (Maybe[Effects]) {
		match this {
			at This[depth: _ lookup: _ source: _] || This[type: _ lookup: _] {
				return this[simplify][trackEffectsIn: ctx]
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
				return this[simplify][applyArgs: args trackEffectsIn: ctx]
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

	on [instanceMembers: from (AnyTypeDecl)] (Array[Member])

	
	;== Method lookup

	on [
		in: ctx (Ctx)
		findStatic: name (Str)
		from: (Type)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleStatic])

	on [
		in: ctx (Ctx)
		findStatic: names (Array[Str])
		from: (Type)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiStatics)

	on [
		in: ctx (Ctx)
		findInstance: name (Str)
		from: (Type)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleInst])

	on [
		in: ctx (Ctx)
		findInstance: names (Array[Str])
		from: (Type)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiInsts)

	on [
		in: ctx (Ctx)
		findCast: target (Type)
		from: (Type)
		cache: (TypeCache)
	] (Casts)

	on [
		in: ctx (Ctx)
		findUnaryOp: op (UnaryOperator.Op)
		from: (Type)
		cache: (TypeCache)
	] (Maybe[UnaryOp])

	on [
		in: ctx (Ctx)
		findBinaryOp: op (BinaryOperator.Op)
		from: (Type)
		cache: (TypeCache)
	] (BinaryOps)


	;== Categories
	
	on [
		findCategory: cat (Type)
		forType: (Type)
		from: (Type)
		cache: (Cache)
	] (Array[Category])

	on [
		findThisCategory: cat (Type)
		from: (Type)
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

type T
category Type for Array[T] {
	on [leastSpecific: by (Func[Type, T])] (This) {
		if this.length < 2 {
			return this[new]
		} else {
			my res = #[this[at: 0]]

			for my i after: 0 upto: this.length {
				my value = this[at: i]
				my res' = res[keepIf: {|value' (T)|
					my parent = by[call: value]
					my child = by[call: value']

					return parent[hasChild: child] || !parent[hasParent: child]
				}]
				if res'.length != res.length {
					res = res'
					res[add: value]
				}
			}

			return res
		}
	}
	
	on [mostSpecific: by (Func[Type, T])] (This) {
		if this.length < 2 {
			return this[new]
		} else {
			my res = #[this[at: 0]]

			for my i after: 0 upto: this.length {
				my value = this[at: i]
				my res' = res[keepIf: {|value' (T)|
					my parent = by[call: value]
					my child = by[call: value']

					return parent[hasParent: child] && !parent[hasChild: child]
				}]
				if res'.length != res.length {
					res = res'
					res[add: value]
				}
			}

			return res
		}
	}
}

category Type for Array[Type] {
	on [leastSpecific] (This) {
		return #inline this[Type leastSpecific: Type$0]
	}

	on [mostSpecific] (This) {
		return #inline this[Type mostSpecific: Type$0]
	}
}