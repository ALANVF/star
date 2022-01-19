alias TypeVarCtx (Dict[TypeVar, Type])

kind Ctx {
	;-- A type decl
	has [decl: (TypeDecl)]

	;-- A category
	has [category: (Category)]
	
	;-- An empty method
	has [emptyMethod: (EmptyMethod)]

	;-- A callable method
	has [method: (RealMethod)]

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
			else {
				match outer at Maybe[the: my outer'] {
					return outer'.typeDecl
				} else {
					throw "impossible!"
				}
			}
		}
	}

	on [typeLookup] (TypeLookup) is getter {
		match this {
			at This[decl: my decl] => return decl
			at This[category: my cat] => return cat
			at This[emptyMethod: my mth] => return mth.decl
			at This[method: my mth] => return mth
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
			thisType: decl.thisType
			:decl
		]
	}

	on [innerCategory: cat (Category)] (This) {
		return This[
			outer: Maybe[the: this]
			thisType: cat.thisType
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

	on [innerObjCascade: type (Type)] (This) {
		return This[
			outer: Maybe[the: this]
			thisType: type
			objCascade: Maybe[the: type]
		]
	}

	on [innerTypevars: typevars (TypeVarCtx)] (This) {
		return This[
			outer: Maybe[the: this]
			:thisType
			:typevars
		]
	}


	;== Errors

	on [addError: diag (Diagnostic)] {
		match this at (
			|| Ctx[decl: my source (HasErrors)]
			|| Ctx[category: my source (HasErrors)]
			|| Ctx[emptyMethod: my source (HasErrors)]
			|| Ctx[method: my source (HasErrors)]
		) {
			source.errors[add: diag]
		} else {
			match outer at Maybe[the: my outer'] {
				outer'[addError: diag]
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
			return local
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
							return Local.Field[ctx: this :member]
						} else {
							return outer'[findLocal: name :depth]
						}
					}

					at This[typeCascade] => throw "todo"

					at This[pattern] => return outer'[findLocal: name :depth] ;@@ TODO

					at This[emptyMethod: my mth (AnyMethod)] || This[method: my mth (AnyMethod)] {
						my isStatic = !this[allowsThis]

						my allMembers = mth.decl[instanceMembers: mth.decl]
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
							at #[my member] => return locals[at: name] = Local.Field[ctx: this :member]
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

			at This[objCascade: _] => return true

			else => match outer at Maybe[the: my outer'] {
				return outer'[allowsThis]
			} else {
				return true
			}
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
				
				return Type[
					type: type'
					args: args[collect: {|arg|
						match arg at Type[depth: my depth lookup: my lookup source: _] {
							match typeLookup[
								findType: lookup
								search: Search.start
								from: typeDecl
								:depth
							] at Maybe[the: my arg'] {
								return arg'
							} else {
								_.this[addError: Error[invalidTypeLookup: arg]]
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
				]
			} else {
				return type
			}
		} else {
			match outer at Maybe[the: my outer'] {
				return outer'[findType: path]
			} else {
				this[addError: Error[invalidTypePath: path]]
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

			at This[code] if isTop => return "{ ... } in \(outer.value[description: false])"

			at This[pattern] if isTop => return "pattern ... in \(outer.value[description: false])"

			at This[objCascade: _] => throw "todo"

			at This[typeCascade] => throw "todo"

			else => return outer.value[description: false]
		}
	}
}