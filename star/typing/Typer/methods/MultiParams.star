use Ident from: Parser

kind MatchParams {
	has [yes]
	has [partial: indexes (Array[Int])]
	has [no]
}

class MultiParam {
	my label (Ident)
	my name (Ident)
	my type (Type)
	my value (Maybe[Parser.Expr]) = Maybe[none]
	my value' (Maybe[Expr]) = Maybe[none]
}

alias MultiParams (Array[MultiParam]) {
	on [matchesNames: names (Array[Str]) isSetter: (Bool) = false] (MatchParams) {
		case {
			;-- exact match
			at this[zip: names all: {|l, n| return (n ?= "=" && isSetter) || l.label.name ?= n}] {
				return MatchParams[yes]
			}

			;-- partial match
			at names.length < this.length && this[any: MultiParam$0.value?] {
				my indexes = Array[Int] #[]
				my namePos = 0
				my thisPos = 0
				my matchedOnce = false
				while namePos < names.length && thisPos < this.length {
					my param = this[at: thisPos]
					case {
						;-- required label
						at param.label.name ?= names[at: namePos] {
							indexes[add: thisPos]
							namePos++
							thisPos++
							matchedOnce ||= true
						}

						;-- label w/ default value
						at param.value? {
							thisPos++
						}

						else {
							matchedOnce = false
							break
						}
					}
				}

				;-- we need to match at least 1 required label
				if matchedOnce {
					return MatchParams[partial: indexes]
				} else {
					return MatchParams[no]
				}
			}
			
			;-- no match
			else => return MatchParams[no]
		}
	}

	on [displayLabels] (Str) {
		return this[collect: MultiParam$0.label.name + ":"][join]
	}
}