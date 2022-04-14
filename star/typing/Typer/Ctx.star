alias TypeVarCtx (Dict[TypeVar, Type])

kind Ctx {
	;-- A type decl
	has [decl: (TypeDecl)] {
		thisType = decl.thisType
	}

	;-- A category
	has [category: (Category)] {
		thisType = category.thisType
	}
	
	;-- An empty method
	has [emptyMethod: (EmptyMethod)]

	;-- A callable method
	has [method: (RealMethod)]

	;-- A member of a type
	has [member: (Member)]

	;-- A tagged case
	has [taggedCase: (TaggedCase)]

	;-- Code of some kind
	has [code]

	;-- A pattern (TODO: should this track the target type?)
	has [pattern]

	;-- Code in an instance cascade
	has [objCascade: (Maybe[Type])]

	;-- Code in a static (type) cascade
	has [typeCascade]

	;-- Context for typevars when doing type checking/inference
	has [typevars: (TypeVarCtx)]


	my outer (Maybe[Ctx]) = Maybe[none]
	my thisType (Type)
	my locals (Dict[Str, Local]) = #()
	my labels (Dict[Str, Stmt]) = #()


	on [typeDecl] (AnyTypeDecl) is getter {
		match this {
			at This[decl: my decl] => return decl
			at This[category: my cat] => return cat
			at This[emptyMethod: my mth] => return mth.decl
			at This[method: my mth] => return mth.decl
			at This[member: my mem] => return mem.decl
			at This[taggedCase: my tcase] => return tcase.decl
			else {
				match outer at Maybe[the: my outer'] {
					return outer'.typeDecl
				} else {
					throw "impossible!"
				}
			}
		}
	}

	on [typeLookup] (TypeLookupDecl) is getter {
		match this {
			at This[decl: my decl] => return decl
			at This[category: my cat] => return cat
			at This[emptyMethod: my mth] => return mth.decl
			at This[method: my mth] => return mth
			at This[member: my mem] => return mem.decl
			at This[taggedCase: my tcase] => return tcase.decl
			else {
				match outer at Maybe[the: my outer'] {
					return outer'.typeLookup
				} else {
					throw "impossible!"
				}
			}
		}
	}


	on [innerDecl: decl (TypeDecl)] (This) {
		return This[
			outer: Maybe[the: this]
			:decl
		]
	}

	on [innerCategory: cat (Category)] (This) {
		return This[
			outer: Maybe[the: this]
			category: cat
		]
	}

	on [innerEmptyMethod: method (EmptyMethod)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			emptyMethod: method
		]
	}

	on [innerMethod: method (RealMethod)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			:method
		]
	}

	on [innerMember: member (Member)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			:member
		]
	}

	on [innerTaggedCase: taggedCase (TaggedCase)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			:taggedCase
		]
	}

	on [innerCode] (This) {
		return This[code]
		-> outer = Maybe[the: this]
		-> thisType = thisType
	}

	on [innerPattern] (This) {
		return This[pattern]
		-> outer = Maybe[the: this]
		-> thisType = thisType
	}

	on [innerObjCascade: type (Maybe[Type])] (This) {
		return This[
			outer: Maybe[the: this]
			thisType: type[orElse: thisType] ;@@ TODO: fix
			objCascade: type
		]
	}

	on [innerTypeCascade: type (Type)] (This) {
		return This[typeCascade]
		-> outer = Maybe[the: this]
		-> thisType = type
	}

	on [innerTypevars: typevars (TypeVarCtx)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			:typevars
		]
	}


	;== Errors

	on [addError: error (Error)] {
		match this at (
			|| Ctx[decl: my source (HasErrors)]
			|| Ctx[category: my source (HasErrors)]
			|| Ctx[emptyMethod: my source (HasErrors)]
			|| Ctx[method: my source (HasErrors)]
			|| Ctx[member: my source (HasErrors)]
			|| Ctx[taggedCase: my source (HasErrors)]
		) {
			source.errors[add: error]
		} else {
			match outer at Maybe[the: my outer'] {
				outer'[addError: error]
			} else {
				throw "Cannot add error to unknown context source!"
			}
		}
	}


	;== Label lookup

	on [findLabel: label (Str)] (Maybe[Stmt]) {
		match labels[maybeAt: label] at Maybe[the: my res] {
			return Maybe[the: res]
		} else {
			match outer at Maybe[the: my outer'] {
				return outer'[findLabel: label]
			} else {
				return Maybe[none]
			}
		}
	}


	;== Variable lookup

	on [findLocal: name (Str) depth: (Int) = 0] (Maybe[Local]) {
		match locals[maybeAt: name] at Maybe[the: my local] if depth ?= 0 || (depth--, false) {
			return Maybe[the: local]
		} else {
			match outer at Maybe[the: my outer'] {
				match this {
					at This[objCascade: Maybe[the: my objType]] {
						match objType[
							in: this
							findInstance: name
							from: outer'.typeDecl
							isGetter: true
						] at SingleInst[member: my member] if depth ?= 0 || (depth--, false) {
							return Maybe[the: Local.Field[ctx: this :member]]
						} else {
							return outer'[findLocal: name :depth]
						}
					}

					at This[typeCascade] => throw "todo"

					at This[pattern] => return outer'[findLocal: name :depth] ;@@ TODO

					at (
						|| This[emptyMethod: AnyMethod[decl: my decl]]
						|| This[method: AnyMethod[decl: my decl]]
						|| This[member: Member[decl: my decl]]
						|| This[taggedCase: TaggedCase[decl: my decl]]
					) {
						my isStatic = !this[allowsThis]

						my allMembers = decl[instanceMembers: decl]
						-> [InPlace keepIf: {|member (Member)|
							return member.name ?= name && (
								|| !isStatic
								|| member.isStatic
								|| {
									match member.decl at (Module) {
										return true
									} else {
										return false
									}
								}
							)
						}]
						-> [InPlace unique]

						match allMembers[Type mostSpecific: Member$0.type.value] {
							at #[] => return Maybe[none]
							at #[my member] => return Maybe[the: locals[at: name] = Local.Field[ctx: this :member]]
							at my members => throw "todo"
						}
					}

					else => return outer'[findLocal: name :depth]
				}
			} else {
				return Maybe[none]
			}
		}
	}

	on [allowsThis] (Bool) {
		match this {
			at This[decl: (Module)] => return false
			at This[decl: _] => return true

			at This[category: _] => return true ; meh

			at This[emptyMethod: (StaticInit) || (StaticDeinit)] => return false
			
			at This[method: (StaticMethod)] => return false

			at This[member: my mem] => return !mem.isStatic && {
				match outer at Maybe[the: my outer'] {
					return outer'[allowsThis]
				} else {
					return true
				}
			}

			at This[taggedCase: _] => return true

			at This[objCascade: _] => return true

			else => match outer at Maybe[the: my outer'] {
				return outer'[allowsThis]
			} else {
				return true
			}
		}
	}

	on [canAssignReadonlyField] (Bool) {
		match this {
			at This[emptyMethod: (DefaultInit) || (StaticDeinit)] => return true
			
			at This[method: (Init)] => return true

			at This[taggedCase: _] => return true

			at This[code] || This[pattern] || This[typevars: _] {
				match outer at Maybe[the: my outer'] {
					return outer'[canAssignReadonlyField]
				} else {
					return false
				}
			}

			else => return false
		}
	}


	;== Type lookup

	on [findType: path (TypePath)] (Maybe[Type]) {
		my found (Maybe[Type]), match this {
			at This[objCascade: Maybe[the: my objType]] {
				#{my depth, my path'} = path[toLookupPath: this.typeLookup]
				match objType[
					findType: path'
					search: Search.start
					from: Maybe[the: this.typeDecl]
					:depth
				] at Maybe[the: found = _] {} else {
					return outer.value[findType: path]
				}
			}

			at This[decl: _] <= _ <= This[method: _] {
				#{my depth, my path'} = path[toLookupPath: this.typeLookup]
				found = this.typeLookup[
					findType: path'
					search: Search.start
					from: Maybe[the: this.typeDecl]
					:depth
				]
			}

			else => return outer.value[findType: path]
		}

		match found at Maybe[the: my type] {
			match type at Type[type: my type' args: my args] {
				my typeDecl = this.typeDecl
				my typeLookup = this.typeLookup
				
				return Maybe[the: Type[
					type: type'
					args: args[collect: {|arg (Type)|
						match arg at Type[depth: my depth lookup: my lookup source: _] {
							match typeLookup[
								findType: lookup
								search: Search.start
								from: Maybe[the: typeDecl]
								:depth
							] at Maybe[the: my arg'] {
								return arg'
							} else {
								;[_.]this[addError: TypeError[invalidTypeLookup: arg.span.value]]
								return arg
							}
						} else {
							return arg
						}
					}]
					span: {
						if type.span? {
							return type.span
						} else {
							return type'.span
						}
					}
				]]
			} else {
				return Maybe[the: type]
			}
		} else {
			match outer at Maybe[the: my outer'] {
				return outer'[findType: path]
			} else {
				this[addError: TypeError[invalidTypeLookup: path.span]]
				return Maybe[none]
			}
		}
	}

	on [findTypevar: typevar (TypeVar)] (Maybe[Type]) {
		match this at This[typevars: my typevars] {
			return typevars[maybeAt: typevar]
		} else {
			match outer at Maybe[the: my outer'] {
				return outer'[findTypevar: typevar]
			} else {
				return Maybe[none]
			}
		}
	}

	on [findTypevarOf: typevar (TypeVar)] (Maybe[TypeVar]) {
		match this at This[typevars: my typevars] {
			for my tvar, my type in: typevars {
				match type at Maybe[the: Type[:typevar]] {
					return Maybe[the: tvar]
				}
			}

			return Maybe[none]
		} else {
			match outer at Maybe[the: my outer'] {
				return outer'[findTypevarOf: typevar]
			} else {
				return Maybe[none]
			}
		}
	}


	;== Describing

	on [description] (Str) is getter => return this[description: true]
	on [description: isTop (Bool)] (Str) is hidden {
		match this {
			at This[decl: my decl] => return "\(decl.declName) \(decl.fullName)"
			
			at This[category: _] => throw "todo"
			
			at This[emptyMethod: my mth] => return "\(mth.declName) for \(outer.value[description: false])"

			at This[method: my mth] {
				my sig = {
					match mth at (Operator) {
						return "`\(mth.methodName)`"
					} else {
						return "[\(mth.methodName)]"
					}
				}
				
				return "\(mth.declName) \(sig) for \(outer.value[description: false])"
			}

			at This[member: my mem] => return "member `\(mem.name)` for \(outer.value[description: false])"
			
			at This[taggedCase: my tcase] => return "tagged case [...] for \(outer.value[description: false])"

			at This[code] if isTop => return "{ ... } in \(outer.value[description: false])"

			at This[pattern] if isTop => return "pattern ... in \(outer.value[description: false])"

			at This[objCascade: _] => throw "todo"

			at This[typeCascade] => throw "todo"

			else => return outer.value[description: false]
		}
	}
}