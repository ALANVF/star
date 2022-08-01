use Core
use Native

category Power for Dec {
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
							if point => exponent--
						} else {
							digits++
							if digits > 18 {
								if !point => exponent++
							} else {
								coef *= Int64 10
								if point => exponent--
							}
						}
					}
					at #"1" <= _ <= #"9" {
						ok = true
						leading = false
						digits++
						if digits > 18 {
							if !point => exponent++
						} else {
							coef = coef * Int64 10 + [c - #"0" Int64]
							if point => exponent--
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

		if ok {
			return Dec[Native
				coefficient: sign * coef
				exponent: exponent[Int8]
			]
		} else {
			return Dec.nan
		}
	}
}