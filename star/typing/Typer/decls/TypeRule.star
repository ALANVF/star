kind TypeRule {
	has [type: l (Type) eq: r (Type)]
	has [type: l (Type) of: r (Type)]
	has [type: l (Type) lt: r (Type)]
	has [type: l (Type) le: r (Type)]
	has [all: conds (Array[TypeRule])]
	has [any: conds (Array[TypeRule])]
	has [one: conds (Array[TypeRule])]
	has [not: rule (TypeRule)]
	has [negate: t (Type)]
	has [exists: t (Type)]


	on [eval: ctx (Ctx)] (Maybe[Effects]) {
		match this {
			at This[type: my l eq: my r] {
				if l[in: ctx][strictUnifyWith: r[in: ctx]] {
					return Maybe[the: Effects.empty]
				} else {
					return Maybe[none]
				}
			}

			at This[type: my l of: my r] {
				if l[in: ctx][hasParent: r[in: ctx]] {
					return Maybe[the: Effects.empty]
				} else {
					return Maybe[none]
				}
			}

			at This[type: my l lt: my r] {
				my l' = l[in: ctx]
				my r' = r[in: ctx]
				
				if l' != r' && l'[hasStrictChild: r'] {
					return Maybe[the: Effects.empty]
				} else {
					return Maybe[none]
				}
			}

			at This[type: my l le: my r] {
				if l[in: ctx][hasStrictChild: r[in: ctx]] {
					return Maybe[the: Effects.empty]
				} else {
					return Maybe[none]
				}
			}

			at This[all: my conds] {
				my effects = Effects.empty

				for my cond in: conds {
					match cond[eval: ctx] at Maybe[the: my effects'] {
						effects += effects'
					} else {
						return Maybe[none]
					}
				}

				return Maybe[the: effects]
			}

			at This[any: my conds] {
				my anyAreTrue = false
				my effects = IEffects #[]

				for my cond in: conds {
					match cond[eval: ctx] at Maybe[the: my effects'] {
						if effects'? {
							effects += effects'
						}
						
						anyAreTrue ||= true
					}
				}

				if anyAreTrue {
					return Maybe[the: Effects.empty + Effect[union: effects]]
				} else {
					return Maybe[none]
				}
			}

			at This[one: my conds] {
				throw "Not yet implemented!"
			}

			at This[not: my rule] {
				match rule[eval: ctx] at Maybe[the: my effects] {
					return Maybe[the: effects[negate]]
				} else {
					return Maybe[none]
				}
			}

			at This[negate: my type] {
				if This[exists: type][eval: ctx]? {
					return Maybe[none]
				} else {
					return Maybe[the: Effects.empty]
				}
			}

			at This[exists: my type] {
				return type[trackEffectsIn: ctx]
			}
		}
	}
}