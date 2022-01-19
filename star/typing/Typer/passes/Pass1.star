;[
==| This pass resolves basic types and stuff that aren't inside an expr, block, or statement.
==| Basic types are types that can be resolved from a simple type path, or are already concrete:
==| - TPath
==| - TLookup (sometimes)
==| - TModular (TODO)
==| - TConcrete, which stays concrete
==| - TApplied
==|   - If there are generic arguments (TApplied), they are ignored when doing lookup
==|     except when checking for arity if the base type is concrete (although the args themselves are still resolved).
]

module Pass1 {
	on [resolve: dir (Dir)] {
		for my file in: dir.files => This[resolve: file]
		for my unit in: dir.units => This[resolve: unit]
	}

	on [resolve: proj (Project)] {
		This[resolve: proj[Dir]]
		match proj.main at Maybe[the: my main] => This[resolve: main]
	}

	on [resolve: unit (Unit)] {
		match unit.primary at Maybe[the: my file] => This[resolve: file]
		This[resolve: unit[Dir]]
	}

	on [resolve: file (File)] {
		;[for my import in: file.imports {
			; ...
		}]

		for my decl in: file.decls => This[resolve: decl]
		for my cat in: file.categories => This[resolve: cat]
	}

	on [resolve: decl (TypeDecl)] {
		for my i, my f in: decl.friends {
			match This[resolve: f from: decl] at Maybe[the: my type] {
				decl.friends[at: i] = type
			}
		}

		match decl.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: decl] at Maybe[the: my type] {
				decl.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		for _, my typevar in: decl.typevars => This[resolve: typevar]
		for my i, my param in: decl.params {
			match This[resolve: param from: decl] at Maybe[the: my type] {
				decl.params[at: i] = type
			}
		}

		match decl at my ns (Namespace) {
			match ns.sealed at Maybe[the: Maybe[the: my type]] {
				match This[resolve: type from: ns] at Maybe[the: my type'] {
					ns.sealed = Maybe[the: Maybe[the: type']]
				}
			}

			for my i, my parent in: ns.parents {
				match This[resolve: parent from: ns] at Maybe[the: my type] {
					ns.parents[at: i] = type
				}
			}
		}

		match decl {
			at my kind = ValueKind[repr: Maybe[the: my repr]] {
				match This[resolve: repr from: decl] at Maybe[the: my type] {
					kind.repr = Maybe[the: type]
				}
			}
			at my alias = RealAlias[type: my type] {
				match This[resolve: type from: decl] at Maybe[the: my type'] {
					alias.type = type'
				}
			}
			at my class = Class[native: Maybe[the: Native[ptr: my t]]] {
				match This[resolve: t from: class] at Maybe[the: my type] {
					class.native = Maybe[the: Native[ptr: type]]
				}
			}
		}

		;@@ TODO: generalize this somehow
		match decl {
			at my ns (Namespace) {
				for my decl' in: ns.decls => This[resolve: decl']
				for my cat in: ns.categories => This[resolve: cat]
				
				for my m in: ns.staticMembers => This[resolve: m]
				for my m in: ns.staticMethods => This[resolve: m]
				match ns at my cl (ClassLike) {
					for my m in: cl.members => This[resolve: m]
					for my m in: cl.methods => This[resolve: m]
					for my o in: cl.operators => This[resolve: o]
					match cl {
						at Class[inits: my inits] || Protocol[inits: my inits] {
							for my i in: inits => This[resolve: i]
						}
						at my kind (TaggedKind) {
							for my c in: kind.taggedCases => This[resolve: c]
						}
					}
				}
			}
			at (
				|| StrongAlias[staticMethods: my smethods methods: my methods operators: my ops]
				|| OpaqueAlias[staticMethods: my smethods methods: my methods operators: my ops]
			) {
				for my m in: smethods => This[resolve: m]
				for my m in: methods => This[resolve: m]
				for my o in: ops => This[resolve: o]
			}
		}
		
		for my ref in: decl.refinements => This[resolve: ref]
	}

	on [resolve: cat (Category)] {
		for my i, my f in: cat.friends {
			match This[resolve: f from: cat] at Maybe[the: my type] {
				cat.friends[at: i] = type
			}
		}

		match cat.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: cat] at Maybe[the: my type] {
				cat.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		for _, my typevar in: cat.typevars => This[resolve: typevar]

		match This[resolve: cat.path from: cat] at Maybe[the: my type] {
			cat.path = type
		}

		match cat.type at Maybe[the: my type] {
			match This[resolve: type from: cat] at Maybe[the: my type'] {
				cat.type = Maybe[the: type']
			}
		}

		for my m in: cat.staticMembers => This[resolve: m]
		for my m in: cat.staticMethods => This[resolve: m]
		for my m in: cat.methods => This[resolve: m]
		for my i in: cat.inits => This[resolve: i]
		for my o in: cat.operators => This[resolve: o]
	}

	on [resolve: typevar (TypeVar)] {
		for my i, my param in: typevar.params {
			match This[resolve: param from: typevar] at Maybe[the: my type] {
				typevar.params[at: i] = type
			}
		}
		
		for my i, my parent in: typevar.parents {
			match This[resolve: parent from: typevar] at Maybe[the: my type] {
				typevar.parents[at: i] = type
			}
		}

		match typevar.native at Maybe[the: Native[ptr: my t]] {
			match This[resolve: t from: typevar] at Maybe[the: my type] {
				typevar.native = Maybe[the: Native[ptr: type]]
			}
		}

		match typevar.rule at Maybe[the: my rule] {
			typevar.rule = This[resolve: rule from: typevar]
		}

		for my cat in: typevar.categories => This[resolve: cat]

		for my m in: typevar.staticMembers => This[resolve: m]
		for my m in: typevar.staticMethods => This[resolve: m]
		for my m in: typevar.members => This[resolve: m]
		for my m in: typevar.methods => This[resolve: m]
		for my i in: typevar.inits => This[resolve: i]
		for my o in: typevar.operators => This[resolve: o]
		for my c in: typevar.taggedCases => This[resolve: c]
	}

	on [resolve: method (RealMethod)] {
		match method {
			at my mth (Method) => This[resolve: mth]
			at my mth (StaticMethod) => This[resolve: mth]
			at my init (Init) => This[resolve: init]
			at my oper (Operator) => This[resolve: oper]
			else => throw "error!"
		}
	}

	on [resolve: method (Method)] {
		match method.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: method] at Maybe[the: my type] {
				method.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		match method {
			at my multi (MultiMethod) {
				for my typevar in: multi.typevars => This[resolve: typevar]
				for my param in: multi.params {
					match This[resolve: param.type from: method] at Maybe[the: my type] {
						param.type = type
					}
				}
			}
			at my cast (CastMethod) {
				for my typevar in: cast.typevars => This[resolve: typevar]
				match This[resolve: cast.type from: method] at Maybe[the: my type] {
					cast.type = type
				}
				return
			}
		}

		match method.ret at Maybe[the: my r] {
			match This[resolve: r from: method] at Maybe[the: my type] {
				method.ret = Maybe[the: type]
			}
		}
	}

	on [resolve: method (StaticMethod)] {
		match method.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: method] at Maybe[the: my type] {
				method.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		match method at my multi (MultiStaticMethod) {
			for my typevar in: multi.typevars => This[resolve: typevar]
			for my param in: multi.params {
				match This[resolve: param.type from: method] at Maybe[the: my type] {
					param.type = type
				}
			}
		}

		match method.ret at Maybe[the: my r] {
			match This[resolve: r from: method] at Maybe[the: my type] {
				method.ret = Maybe[the: type]
			}
		}
	}

	on [resolve: init (Init)] {
		match init.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: init] at Maybe[the: my type] {
				init.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		match init at my multi (MultiInit) {
			for my typevar in: multi.typevars => This[resolve: typevar]
			for my param in: multi.params {
				match This[resolve: param.type from: init] at Maybe[the: my type] {
					param.type = type
				}
			}
		}
	}

	on [resolve: op (Operator)] {
		match op.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: op] at Maybe[the: my type] {
				op.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		match op at my binop (BinaryOperator) {
			for my typevar in: binop.typevars => This[resolve: typevar]
			match This[resolve: binop.paramType from: op] at Maybe[the: my type] {
				binop.paramType = type
			}
		}

		match op.ret at Maybe[the: my r] {
			match This[resolve: r from: op] at Maybe[the: my type] {
				op.ret = Maybe[the: type]
			}
		}
	}

	on [resolve: member (Member)] {
		match member.hidden at Maybe[the: Maybe[the: my h]] {
			match This[resolve: h from: member.decl] at Maybe[the: my type] {
				member.hidden = Maybe[the: Maybe[the: type]]
			}
		}

		match member.type at Maybe[the: my t] {
			match This[resolve: t from: member.decl] at Maybe[the: my type] {
				member.type = type
			}
		}
	}

	on [resolve: tcase (TaggedCase)] {
		match tcase at TaggedCase.Multi[params: my params] {
			for my i, my param in: params {
				match This[resolve: param.type from: tcase.decl] at Maybe[the: my type] {
					params[at: i] = type
				}
			}
		}
	}

	on [resolve: rule (TypeRule) from: (TypeLookup)] (TypeRule) {
		match rule {
			at TypeRule[negate: my t] => return TypeRule[negate: This[resolve: t :from][orElse: t]]
			at TypeRule[exists: my t] => return TypeRule[exists: This[resolve: t :from][orElse: t]]
			at TypeRule[type: my l eq: my r] => return TypeRule[type: This[resolve: l :from][orElse: l] eq: This[resolve: r :from][orElse: r]]
			at TypeRule[type: my l of: my r] => return TypeRule[type: This[resolve: l :from][orElse: l] of: This[resolve: r :from][orElse: r]]
			at TypeRule[type: my l lt: my r] => return TypeRule[type: This[resolve: l :from][orElse: l] lt: This[resolve: r :from][orElse: r]]
			at TypeRule[type: my l le: my r] => return TypeRule[type: This[resolve: l :from][orElse: l] le: This[resolve: r :from][orElse: r]]
			at TypeRule[not: my r] => return TypeRule[not: This[resolve: r :from]]
			at TypeRule[all: my rs] => return TypeRule[all: rs[collect: This[resolve: Type$.0 :from][orElse: $.0]]]
			at TypeRule[any: my rs] => return TypeRule[any: rs[collect: This[resolve: Type$.0 :from][orElse: $.0]]]
			at TypeRule[one: my rs] => return TypeRule[one: rs[collect: This[resolve: Type$.0 :from][orElse: $.0]]]
		}
	}

	on [resolve: type (Type) from: source (TypeLookup) cache: (Cache) = Cache[new]] (Maybe[Type]) {
		match type {
			at Type[depth: my depth lookup: my path source: my source'] {
				for #{_, _, my args} in: path {
					if args? {
						for my i, my arg in: args {
							args[at: i] = This[resolve: arg from: source :cache]
						}
					}
				}
				
				match source[findType: path search: Search.start from: Maybe[none] :depth :cache] at Maybe[the: my type'] {
					return Maybe[the: type']
				} else {
					if source != source' {
						match source'[findType: path search: Search.start from: Maybe[none] :depth :cache] at Maybe[the: my type'] {
							return Maybe[the: type']
						} else {
							source'.errors[add: Error[invalidTypeLookup: type]]
							return Maybe[none]
						}
					} else {
						source.errors[add: Error[invalidTypeLookup: type]]
						return Maybe[none]
					}
				}
			}
			at Type[type: my base lookup: my path] {
				match This[resolve: base from: source :cache] {
					at Maybe[the: my base' = Type[decl: my decl] ;[|| Type[type: _ unit: _]]] {
						cache += base'
						; maybe we should use the cache here?
						match decl[findType: path search: Search.inside from: Maybe[none]] at Maybe[the: my type'] {
							return Maybe[the: type']
						} else {
							if source != decl {
								cache += decl
								match source[findType: path search: Search.start from: Maybe[none] :cache] at Maybe[the: my type'] {
									return Maybe[the: type']
								} else {
									source.errors[add: Error[invalidTypeLookup: type]]
									return Maybe[none]
								}
							} else {
								decl.errors[add: Error[invalidTypeLookup: type]]
								return Maybe[none]
							}
						}
					}
					at Maybe[the: _] => return Maybe[the: type] ;@@ TODO?
					else => return Maybe[none]
				}
			}
			at Type[type: my base args: my args] {
				match This[resolve: base from: source :cache] at Maybe[the: my base'] {
					my args' = #[]
					for my arg in: args {
						match This[resolve: arg from: source :cache] at Maybe[the: my arg'] {
							args'[add: arg']
						} else {
							return Maybe[none]
						}
					}

					return Maybe[the: Type[type: base' args: args']]
				} else {
					return Maybe[none]
				}
			}
			else => return Maybe[the: type]
		}
	}
}