use Core
use Native

category Power for Dec {
	;-- Largest possible base 10 exponent
	my max_exp is static is hidden is readonly = 511

	;-- Table giving binary powers of 10
	my powers_of_10 is static is hidden is readonly = #[
		1e1
		1e2
		1e4
		1e8
		1e16
		1e32
		1e64
		1e128
		1e256
	]

	;-- Taken from here: https://github.com/vpisarev/DEC64/blob/alt/dec64_string.c#L356
	on [fromStr: str (Str)] (Dec) is static {
		my pos (Int)
		my c (Char)
		my digits = 0
		my leading = true
		my ok = false
		my point = false

		my coef = Int64 0
		my exp = Int64 0
		my exponent = Int64 0
		my sign (Int64)
		my signExp = Int64 0

		if str.length ?= 0 => return Dec.nan

		c = str[at: 0]

		if c ?= #"-" {
			c = str[at: 1]
			pos = 1
			sign = Int64 -1
		} else {
			pos = 0
			sign = Int64 1
		}

		while pos < str.length {
			if c != #"_" {
				match c {
					at #"0" {
						ok = true
						if leading {
							exponent -= point[Int64]
						} else {
							digits++
							if digits > 18 {
								exponent += Int64 1 - point[Int64]
							} else {
								coef *= Int64 10
								exponent -= point[Int64]
							}
						}
					}
					at #"1" <= _ <= #"9" {
						ok = true
						leading = false
						digits++
						if digits > 18 {
							exponent += Int64 1 - point[Int64]
						} else {
							coef = coef * Int64 10 + [c - #"0" Int64]
							exponent -= point[Int64]
						}
					}
					at #"." || #"," {
						if point => return Dec.nan
						point = true
					}
					at #"e" || #"E" {
						if ok {
							ok = false
							exp = Int64 0
							signExp = Int64 1
							pos++
							c = str[at: pos]
							
							match c {
								at #"-" {
									signExp = Int64 -1
									pos++
									c = str[at: pos]
								}
								at #"+" {
									pos++
									c = str[at: pos]
								}
							}

							while pos < str.length {
								if #"0" <= c <= #"9" {
									ok = true
									exp = exp * Int64 10 + [c - #"0" Int64]
									if exp < Int64 0 => return Dec.nan
								} else {
									return Dec.nan
								}

								pos++
								c = str[at: pos]
							}
						}

						return Dec[Native
							coefficient: sign * coef
							exponent: [(signExp * exp) + exponent Int8]
						]
					}
					else {
						return Dec.nan
					}
				}
			}

			pos++
			c = str[at: pos]
		}
	}

;[
	;-- Taken from here: https://github.com/oss-forks/strtod/blob/master/strtod.c
	on [fromStr: str (Str)] (Dec) is static {
		my sign = false, my expSign = false
		my fraction = 0.0, my dblExp = 0.0
		my p = 0
		my c = #"\0"
		my exp = 0
		my fracExp = 0

		my mantSize = 0
		my decPt = 0

		my pExp = 0

		while str[Unsafe at: p][isSpace] {
			p++
		}
		if str[Unsafe at: p] ?= #"-" {
			sign = true
			p++
		} else {
			if str[Unsafe at: p] ?= #"+" {
				p++
			}
			sign = false
		}

		
		decPt = -1
		while true {
			c = str[Unsafe at: p]
			if !c[isDigit] {
				if c != #"." || decPt >= 0 {
					break
				}
				decPt = mantSize
			}
			mantSize++
		}


		do label: `done` {
			pExp = p
			p -= mantSize
			if decPt < 0 {
				decPt = mantSize
			} else {
				mantSize--
			}
			if mantSize > 18 {
				fracExp = decPt - 18
				mantSize = 18
			} else {
				fracExp = decPt - mantSize
			}
			if mantSize ?= 0 {
				fraction = 0.0
				p = 0
				break `done`
			} else {
				my frac1 = 0, my frac2 = 0
				while mantSize > 9 {
					c = str[Unsafe at: p]
					p++
					if c ?= #"." {
						c = str[Unsafe at: p]
						p++
					}
					frac1 = 10*frac1 + [c - #"0" Int]
					mantSize--
				}
				while mantSize > 0 {
					c = str[Unsafe at: p]
					p++
					if c ?= #"." {
						c = str[Unsafe at: p]
						p++
					}
					frac2 = 10*frac2 + [c - #"0" Int]
				}
				fraction = (1e9 * frac1[Dec]) + frac2[Dec]
			}


			p = pExp
			if str[Unsafe at: p] ?= #"E" || str[Unsafe at: p] ?= #"e" {
				p++
				if str[Unsafe at: p] ?= #"-" {
					expSign = true
					p++
				} else {
					if str[Unsafe at: p] ?= #"+" {
						p++
					}
					expSign = false
				}
				while str[Unsafe at: p][isDigit] {
					exp = exp * 10 + [str[Unsafe at: p] - #"0" Int]
					p++
				}
			}
			if expSign {
				exp = fracExp - exp
			} else {
				exp = fracExp + exp
			}

			
			if exp < 0 {
				expSign = true
				exp = -exp
			} else {
				expSign = false
			}
			if exp > max_exp {
				throw "Exponent out of range!"
			}
			dblExp = 1.0
			for my d in: powers_of_10 while: exp != 0 {
				if (exp & 01)? {
					dblExp *= d
				}
				exp >>= 1
			}
			if expSign {
				fraction /= dblExp
			} else {
				fraction *= dblExp
			}
		}

		if sign {
			return -fraction
		} else {
			return fraction
		}
	}]
}