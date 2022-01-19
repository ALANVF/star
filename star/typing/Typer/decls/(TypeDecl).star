use Ident from: Parser

protocol TypeDecl of AnyTypeDecl {
	my typevars (TypeVars) is getter = TypeVars #()

	my ident (Ident) is getter
	my params (Array[Type])

	my hidden (Maybe[Maybe[Type]]) = Maybe[none]
	my friends (Array[Type]) is getter = #[]
	
	my refinements (Array[TypeDecl]) is getter = #[]
	my refinees (Array[TypeDecl]) is getter = #[]

	init {
		thisType = Type[decl: this]
	}


	on [name] (Str) is getter => return ident.name


	;== Display info
	
	on [fullName: cache (TypeCache)] (Str) {
		cache += thisType

		my res = {
			match this[Type fullPath] at Maybe[the: my path] {
				return path
			} else {
				throw "Not a real type???"
			}
		}

		if params? {
			res
			-> [add: "["]
			-> [add: params[collect: {|param|
				if cache[contains: param] {
					return "..."
				} else {
					return param[fullName: cache]
				}
			}][joinWith: ", "]]
			-> [add: "]"]
		}

		return res
	}


	;== Errors

	on [hasErrors] (Bool) {
		return (
			|| this[Super[HasErrors] hasErrors]
			|| typevars.values[any: TypeVar$0[hasErrors]]
		)
	}

	on [allErrors] (Array[Diagnostic]) {
		return this[Super[HasErrors] allErrors]
		-> [addAll: typevars.values[collectAll: TypeVar$0[allErrors]]]
	}


	;== Type refinement

	on [buildRefinements] {
		if params? {
			;-- Find all refinements (excluding the current decl)
			match lookup[
				findType: LookupPath #[#{ident.span, ident.name, params}]
				search: Search.inside
				from: Maybe[the: this]
				cache: Cache[new] + thisType
			] at Maybe[the: my found] {
				match found {
					at Type[decl: my decl] || Type[type: Type[decl: my decl] args: _] {
						if (
							&& this != decl
							&& this.name ?= decl.name
							&& lookup ?= decl.lookup
							&& params[zip: decl.params all: Type$0[hasChild: Type$.1]]
							&& !decl.refinements[contains: this]
						) {
							refinements[add: decl]
							decl.refinees[add: this]
						}
					}

					at Type[type: Type[types: my types] args: _] {
						for my type in: types {
							match type at Type[decl: my decl] || Type[type: Type[decl: my decl] args: _] {
								if (
									&& this != decl
									&& this.name ?= decl.name
									&& lookup ?= decl.lookup
									&& params[zip: decl.params all: Type$0[hasChild: Type$.1]]
									&& !decl.refinements[contains: this]
								) {
									refinements[add: decl]
									decl.refinees[add: this]
								}
							}
						}
					}

					else {}
				}
			}
		}
	}


	;== Type lookup

	on [makeTypePath: path (TypePath)] (Type) {
		return path[toType: this]
	}
	
	on [
		findType: path (LookupPath)
		search: (Search)
		from: (Maybe[AnyTypeDecl])
		depth: (Int)
		cache: (Cache)
	] (Maybe[Type]) {
		if !from {
			from = Maybe[the: this]
		}

		; temp until I get destructuring working
		;[my head = path[at: 0]
		my rest = path[after: 0]]
		#[my head, ...my rest] = path
		match head {
			at #{my span, "This", my args} if search != Search.inside && depth ?= 0 {
				if args? {
					match params.length {
						at 0 {
							errors[add: Error[invalidTypeApply: span.value because: "Attempt to apply arguments to a non-parametric type"]]
							return Maybe[none]
						}

						at _ < args.length {
							errors[add: Error[invalidTypeApply: span.value because: "Too many arguments"]]
							return Maybe[none]
						}

						at _ > args.length {
							errors[add: Error[invalidTypeApply: span.value because: "Not enough arguments"]]
							return Maybe[none]
						}

						else => return Maybe[the: Type[type: Type[:this :span] :args :span]]
					}
				} else {
					return Maybe[the: Type[:this :span]]
				}
			}

			at #{my span, my name, my args} {
				my finished = true
				my res = {
					if search ?= Search.inside => return Maybe[none]

					match {
						match typevars[maybeAt: name] at Maybe[the: my found] {
							return Maybe[the: found[keepIf: {|typevar (TypeVar)|
								return !cache[has: typevar.thisType] && (
									|| typevar.params.length ?= 0
									|| typevar.params.length ?= args.length
								)
							}]]
						} else {
							return Maybe[none]
						}
					} {
						at Maybe[none] || Maybe[the: #[]] {
							return lookup[findType: path search: Search.outside :from :depth :cache]
						}

						at Maybe[the: _] if depth != 0 {
							return lookup[findType: path search: Search.outside :from depth: depth - 1 :cache]
						}
						
						at Maybe[the: #[my typevar]] => match #{args, typevar.params} {
							at #{#[], _} {
								finished = false
								return Maybe[the: Type[:typevar :span]]
							}

							at #{_, #[]} {
								; error...?
								return lookup[findType: path search: Search.outside :from :depth :cache]
							}

							at #{_, my params'} => case {
								at args.length > params.length {
									errors[add: Error[invalidTypeApply: span.value because: "Too many arguments"]]
									return Maybe[none]
								}

								at args.length < params.length {
									errors[add: Error[invalidTypeApply: span.value because: "Not enough arguments"]]
									return Maybe[none]
								}

								else {
									finished = false
									return Maybe[the: Type[
										:span
										type: typevar.thisType
										args: args[TypeLookup resolveArgs: from, errors]
									]]
								}
							}
						}

						at Maybe[the: my found] {
							if args.length ?= 0 {
								finished = false
								return Maybe[the: Type[types: found[collect: TypeVar$0.thisType] :span]]
							} else {
								match found[keepIf: TypeVar$0.params.length ?= args.length][collect: TypeVar$0.thisType] {
									at #[] {
										errors[add: Error[invalidTypeApply: span.value because: "No candidate matches the type arguments"]]
										return Maybe[none]
									}

									at #[my type] {
										finished = false
										return Maybe[the: Type[
											:span
											:type
											args: args[TypeLookup resolveArgs: from, errors]
										]]
									}

									at my types {
										finished = false
										return Maybe[the: Type[
											:span
											type: Type[:types :span]
											args: args[TypeLookup resolveArgs: from, errors]
										]]
									}
								}
							}
						}
					}
				}

				match res {
					at Maybe[none] => return lookup[findType: path search: Search.outside :from :depth :cache]
					at _ if finished || !rest => return res
					at Maybe[the: Type[decl: my decl]] => return decl[findType: rest search: Search.inside :from :cache]
					at Maybe[the: my type] => return Type[:type lookup: rest :span]
				}
			}
		}
	}


	;== Type checking

	on [hasParent: decl (TypeDecl)] (Bool) {
		return (
			|| this ?= decl
			|| (decl ?= Pass2.std_Value && !this[isNative: Native[void]])  ;-- If `decl` is Star.Value and this is not void
			|| refinees[any: TypeDecl$0[hasParent: decl]]                          ;-- If any refinements have a parent `decl`
		)
	}

	on [hasParent: type (Type)] (Bool) {
		return (
			|| thisType ?= type
			|| refinees[any: TypeDecl$0[hasParent: type]]  ;-- If any refinees have a parent `type`
			|| type[hasChild: this]                ;-- Check in the opposite direction
		)
	}
	

	on [hasChild: decl (TypeDecl)] (Bool) {
		return (
			|| this ?= decl
			|| refinements[any: TypeDecl$0[hasChild: decl]]  ;-- If any refinements have a child `decl`
		)
	}

	on [hasChild: type (Type)] (Bool) {
		return (
			|| thisType ?= type
			|| (this ?= Pass2.std_Value && !type[isNative: Native[void]])  ;-- If this is Star.Value and `type` is not void
			|| refinees[any: TypeDecl$0[hasChild: type]]                           ;-- If any refinees have a child `type`
			|| type[hasParent: this]                                       ;-- Check in the opposite direction
		)
	}


	on [hasStrictChild: decl (TypeDecl)] (Bool) {
		return (
			|| this ?= decl
			|| refinements[contains: decl]  ;-- If any refinees have a strict child `type`
		)
	}

	on [hasStrictChild: type (Type)] (Bool) {
		if thisType ?= type => return true

		match type {
			at Type[decl: my decl] => return this[hasStrictChild: decl]

			at Type[type: my type' unit: _] => return this[hasStrictChild: type']
			
			; ...
			
			else => return false
		}
	}


	;== Unification

	on [strictUnifyWith: type (Type)] (Maybe[Type]) {
		if thisType ?= type => return Maybe[the: thisType]

		match type {
			at Type[decl: my decl] {
				if this ?= decl {
					return Maybe[the: thisType]
				} else {
					return Maybe[none]
				}
			}

			at Type[type: my type' unit: _] => return this[strictUnifyWith: type']

			; ...

			else => return Maybe[none]
		}
	}


	;== Generics

	on [acceptsArgs: args (Array[Type])] (Bool)

	on [applyArgs: args (Array[Type])] (Maybe[Type]) {
		if args.length != params.length => return Maybe[none]

		my typevarCtx = TypeVarCtx #()
		my params' = Array[Type] #[]

		;-- Expand all typevars by binding the arg to the param
		for #{my param, my arg} in: params[zip: args] {
			match arg[bindTo: param in: typevarCtx] at Maybe[the: my type] {
				params'[add: type]
			} else {
				return Maybe[none]
			}
		}

		return Maybe[the: Type[decl: this params: params' ctx: typevarCtx]]
	}

	
	;== Attributes

	on [isNative: native (Native)] (Bool) => return false

	on [isFlags] (Bool) => return false

	on [isStrong] (Bool) => return false

	on [isUncounted] (Bool) => return false


	;== Iterating

	on [iterElemType] (Maybe[Type]) {
		for my refinee in: refinees {
			;-- this allows us to obtain our typevars from refinee typevars
			my ref = {
				if params? {
					return refinee[applyArgs: params]
				} else {
					return Maybe[the: refinee.thisType]
				}
			}
			match ref at Maybe[the: my ref'] {
				match ref'[iterElemType] at Maybe[the: my elem] {
					return Maybe[the: elem[from: thisType]]
				}
			}
		}

		return Maybe[none]
	}

	on [iterAssocType] (Maybe[Tuple[Type, Type]]) {
		for my refinee in: refinees {
			;-- this allows us to obtain our typevars from refinee typevars
			my ref = {
				if params? {
					return refinee[applyArgs: params]
				} else {
					return Maybe[the: refinee.thisType]
				}
			}
			match ref at Maybe[the: my ref'] {
				match ref'[iterAssocType] at Maybe[the: #{my k, my v}] {
					return Maybe[the: #{
						k[from: thisType]
						v[from: thisType]
					}]
				}
			}
		}

		return Maybe[none]
	}


	;== Effects tracking
	
	on [applyArgs: args (Array[Type]) trackEffectsIn: ctx (Ctx)] (Maybe[Tuple[Type, Effects]]) {
		if args.length != params.length => return Maybe[none]

		my effects = Effects.empty
		my typevarCtx = TypeVarCtx #()
		my params' = Array[Type] #[]

		;-- Expand all typevars by binding the arg to the param
		for #{my param, my arg} in: params[zip: args] {
			match arg[bindTo: param in: typevarCtx] at Maybe[the: my type] {
				params'[add: type]
			} else {
				return Maybe[none]
			}
		}

		;-- then gather up the effects from the expanded params
		my ctx' = ctx[innerTypevars: typevarCtx]
		for my type in: params' {
			match type[trackEffectsIn: ctx'] at Maybe[the: my effects'] {
				effects += effects'
			} else {
				return Maybe[none]
			}
		}

		return Maybe[the: #{
			Type[decl: this params: params' ctx: typevarCtx]
			effects
		}]
	}


	;== Privacy

	on [canSee: member (Member)] (Bool)
	on [canSee: method (RealMethod)] (Bool)


	;== Members

	on [instanceMembers: from (AnyTypeDecl)] (Array[Member]) {
		return refinees[collectAll: TypeDecl$0[instanceMembers: from]]
	}

	
	;== Method lookup

	on [
		in: ctx (Ctx)
		findStatic: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleStatic]) => return Maybe[none]

	on [
		in: ctx (Ctx)
		findStatic: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiStatics) => return #[]

	on [
		in: ctx (Ctx)
		findInstance: name (Str)
		from: (AnyTypeDecl)
		isGetter: (Bool)
		cache: (TypeCache)
	] (Maybe[SingleInst]) => return Maybe[none]

	on [
		in: ctx (Ctx)
		findInstance: names (Array[Str])
		from: (AnyTypeDecl)
		isSetter: (Bool)
		cache: (TypeCache)
	] (MultiInsts) => return #[]

	on [
		in: ctx (Ctx)
		findCast: target (Type)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (Casts) => return #[]

	on [
		in: ctx (Ctx)
		findUnaryOp: op (UnaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (Maybe[UnaryOp]) => return Maybe[none]

	on [
		in: ctx (Ctx)
		findBinaryOp: op (BinaryOperator.Op)
		from: (AnyTypeDecl)
		cache: (TypeCache)
	] (BinaryOps) => return #[]


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