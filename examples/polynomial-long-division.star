class Fraction {
	my top (Poly)
	my bottom (Poly)

	on [Str] {
		return "\(top) / \(bottom)"
	}
}

class PolyRes {
	my poly (Poly)
	my rem (Fraction)

	on [Str] {
		case {
			at rem.top.degree ?= rem.top.terms[at: 0] ?= 0 {
				return poly[Str]
			}

			at poly.degree ?= poly.terms[at: 0] ?= 0 {
				return ""
				-> [add: [rem.top[sign] / rem.bottom[sign] ?= -1 yes: "-(" no: "("]]
				-> [add: Fraction[top: rem.top * rem.top[sign] bottom: rem.bottom * rem.bottom[sign]]]
				-> [add: ")"]
			}
			
			else {
				my sign = rem.top[sign] / rem.bottom[sign]
				return ""
				-> [add: poly]
				-> [add: [sign ?= -1 yes: " - (" no: " + ("]]
				-> [add: Fraction[top: rem.top * rem.top[sign] bottom: rem.bottom * rem.bottom[sign]]]
				-> [add: ")"]
			}
		}
	}
}

class Poly {
	my terms (Array[Num])
	my degree (Int)
	
	init [newWithCoeffs: coeffs (Array[Num])] {
		terms = coeffs
		degree = coeffs.length - 1
	}

	on [sign] (Int) {
		return terms[at: 0][abs]
	}

	operator `/` [poly (Poly)] (Poly) {
		if degree < poly.degree {
			return PolyRes[
				poly: Poly[newWithCoeffs: #[0]]
				rem: Fraction[top: this bottom: poly]
			]
		} else {
			my p = terms[new]
			my factors = #[]
			
			while p.length >= poly.degree {
				my factor = p[at: 0] / poly.terms[at: 0]
				factors[add: factor]
				
				for my i from: 0 upto: poly.term.length {
					p[at: i] -= poly.terms[at: i] * factor
				}

				while p[at: 0] ?= 0 {
					p[removeAt: 0]
				}
			}

			if p.length ?= 0 {
				p = #[0]
			}

			return PolyRes[
				poly: Poly[newWithCoeffs: factors]
				rem: Fraction[top: Poly[newWithCoeffs: p] bottom: poly]
			]
		}
	}

	operator `*` [f (Int)] (Poly) {
		return Poly[newWithCoeffs: terms[collect: $0 * f]]
	}

	on [Str] {
		my out = ""
		
		for my i, my e in: terms {
			if e != 0 {
				if i != 0 {
					out[add: [e < 0 yes: " - " no: " + "]]
				} orif e < 0 {
					out[add: "-"]
				}

				if e[abs] != 1 {
					out[add: e[abs]]
				}

				if i < terms.length - 2 {
					out[add: "x^\(i)"]
				} orif i ?= terms.length - 2 {
					out[add: "x"]
				}
			}
		}

		return out
	}
}

module Main {
	on [main] {
		Core[say: Poly[newWithCoeffs: #[1, -12, 0, -42]] / Poly[newWithCoeffs: #[1, -3]]]
		Core[say: Poly[newWithCoeffs: #[1, -12, 0, -42]] / Poly[newWithCoeffs: #[1, 1, -3]]]
	}
}