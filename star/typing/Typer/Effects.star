alias IEffects = ISet[Effect]

kind Effect {
	has [typevar: (TypeVar) in: ctx (Ctx)]
	has [union: effects (IEffects)]
}

class Effects {
	alias Entries = Immutable.Dict[TypeVar, IEffects]


	my empty is static is readonly = Effects[new]
	
	my adds is getter = Entries #()
	my removes is getter = Entries #()


	on [for: typevar (TypeVar) add: effect (Effect)] (This) {
		match removes[maybeAt: typevar] at Maybe[the: my effects] if effects[contains: effect] {
			throw "Cannot add effect which is already negated!"
		} else {
			return This[
				adds: adds[at: typevar set: {
					match adds[maybeAt: typevar] at Maybe[the: my effects] {
						return effects + effect
					} else {
						return ISet #[effect]
					}
				}]
				:removes
			]
		}
	}

	on [for: typevar (TypeVar) remove: effect (Effect)] (This) {
		match adds[maybeAt: typevar] at Maybe[the: my effects] if effects[contains: effect] {
			throw "Cannot remove effect which already exists!"
		} else {
			return This[
				:adds
				removes: removes[at: typevar set: {
					match removes[maybeAt: typevar] at Maybe[the: my effects] {
						return effects + effect
					} else {
						return ISet #[effect]
					}
				}]
			]
		}
	}


	on [addsFor: typevar (TypeVar)] (IEffects) {
		match adds[maybeAt: typevar] at Maybe[the: my effects] {
			return effects
		} else {
			return IEffects #[]
		}
	}

	on [removesFor: typevar (TypeVar)] (IEffects) {
		match removes[maybeAt: typevar] at Maybe[the: my effects] {
			return effects
		} else {
			return IEffects #[]
		}
	}


	on [negate] (This) {
		return This[
			adds: removes
			removes: adds
		]
	}


	operator `+` [other (This)] (This) {
		case {
			at this ?= empty => return other
			at other ?= empty => return this
			else {
				my adds' = adds[Dict[TypeVar, IEffects]]
				my removes' = removes[Dict[TypeVar, IEffects]]

				for my typevar, my effects in: other.adds {
					;-- make sure `removes` doesn't have any conflicting effects with `adds`
					match removes[maybeAt: typevar] at Maybe[the: my effects'] {
						if effects'[containsAny: effects] {
							throw "Conflicting effects!"
						}
					}

					adds'[at: typevar] = {
						match adds[maybeAt: typevar] at Maybe[the: my effects'] {
							return effects' | effects
						} else {
							return effects
						}
					}
				}
				
				for my typevar, my effects in: other.removes {
					;-- we don't need to check for conflicts because we already did that in the prev loop

					removes'[at: typevar] = {
						match removes[maybeAt: typevar] at Maybe[the: my effects'] {
							return effects' | effects
						} else {
							return effects
						}
					}
				}

				return This[
					adds: adds'[Entries]
					removes: removes'[Entries]
				]
			}
		}
	}
}


;[

type From { on [To] }
type To
alias Castable[From, To]


=== TYPEVAR vs TYPEVAR ===

type T
type T'
if Castable[T, T']?
	EFFECTS:
		IN
			type From { on [To] }
			type To
			alias Castable[From, To]
		WHERE
			From := T
			To   := T'
		THEN
			type T { on [T'] }
			type T'
		SO
			refine T += type { on [T'] }


=== TYPEVAR vs CONCRETE ===

type T
if Castable[T, Int]?
	EFFECTS:
		IN
			type From { on [To] }
			type To
			alias Castable[From, To]
		WHERE
			From := T
			To   := Int
		THEN
			type T { on [Int] }
			erase To because Int is concrete
		SO
			refine T += type { on [Int] }


=== CONCRETE vs TYPEVAR ===

type T'
if Castable[Int, T']?
	EFFECTS:
		IN
			type From { on [To] }
			type To
			alias Castable[From, To]
		WHERE
			From := Int
			To   := T'
		THEN
			erase From because Int is concrete
			type T' if T' ?= (targets of Int casts)
		SO
			update T' += T' & (targets of Int casts)


=== CONCRETE vs CONCRETE ===

if Castable[Int, Dec]?
	EFFECTS:
		IN
			type From { on [To] }
			type To
			alias Castable[From, To]
		WHERE
			From := Int
			To   := Dec
		THEN
			erase From because Int is concrete
			erase To because Dec is concrete
		SO
			check Int & (targets of Dec casts)

]