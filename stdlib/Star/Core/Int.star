class Int of Native.Int32, Num {
	on [sign] (This) {
		case {
			at this < 0 => return -1
			at this > 0 => return 1
			else        => return 0
		}
	}

	on [abs] (This) is native `i32_abs`

	on [sqrt] (Dec) is inline {
		return this[Dec][sqrt]
	}
	
	on [rootOf: root (Num)] (Dec) is inline {
		return this[Dec][rootOf: root]
	}
	
	on [exp] (Dec) is inline {
		return this[Dec][exp]
	}
	
	on [sin] (Dec) is inline {
		return this[Dec][sin]
	}
	
	on [cos] (Dec) is inline {
		return this[Dec][cos]
	}
	
	on [tan] (Dec) is inline {
		return this[Dec][tan]
	}
	
	on [asin] (Dec) is inline {
		return this[Dec][asin]
	}
	
	on [acos] (Dec) is inline {
		return this[Dec][acos]
	}
	
	on [atan] (Dec) is inline {
		return this[Dec][atan]
	}
	
	;on [atan2: (This)] (This)
	
	on [floor] (Dec) is inline {
		return this[Dec][floor]
	}
	
	on [ceiling] (Dec) is inline {
		return this[Dec][ceiling]
	}
	
	on [round] (Dec) is inline {
		return this[Dec][round]
	}
	
	on [round: digits (Int)] (Dec) {
		return this[Dec][round: digits]
	}
	
	on [ln] (Dec) is inline {
		return this[Dec][ln]
	}
	
	on [log] (Dec) is inline {
		return this[Dec][log]
	}

	on [log: base (Int)] (This) is inline {
		return this[Dec][log: base]
	}

	on [gcd: other (This)] (This) {
		my u = this[abs]
		my v = other[abs]
		my shift = 0

		if u ?= 0 {
			return v
		}

		if v ?= 0 {
			return u
		}

		while (u | v) %% 2 {
			shift++
			u >>= 1
			v >>= 1
		}

		while u %% 2 {
			u >>= 1
		}
		
		do {
			while v %% 2 {
				v >>= 1
			}

			if u > v {
				#asm #swap_reg #{u, v}
			}
		} while (v -= u)?

		return u << shift
	}

	on [lcm: other (This)] (This) {
		return this // this[gcd: other] * other
	}

	on [Char] is native `cast_i32_u8`
}