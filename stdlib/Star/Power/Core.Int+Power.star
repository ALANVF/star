use Core

category Power for Int {
	on [
		fromStr: str (Str)
		usesUnifiedPrefix: (Bool) = false
	] (Int) is static {
		#{my index, my sign} = This[Power getSignFromStr: str]
		my base = {
			try {
				#{my base', index} = This[Power getBaseFromStr: str :usesUnifiedPrefix :index]
				return base'
			} catch {
				at _ => return 10
			}
		}

		return This[Power fromStr: str withBase: base :sign :index]
	}

	on [
		fromStr: str (Str)
		withBase: base (Int) = 10
		includesPrefix: (Bool) = true
		usesUnifiedPrefix: (Bool) = false
	] (Int) is static {
		if base < 2 || base > 36 {
			throw "Invalid base: must be 2..36"
		}

		#{my index, my sign} = This[Power getSignFromStr: str]

		if includesPrefix {
			match This[Power getBaseFromStr: str :usesUnifiedPrefix :index] {
				at #{base, index = _} {}
				at #{my base', _} => throw "Base \(base') found, expected base \(base)"
			}
		}

		return This[Power fromStr: str withBase: base :sign :index]
	}

	on [getSignFromStr: str (Str)] (Tuple[Int, Int]) is static is hidden {
		match str[at: 0] {
			at #"+" => return #{1, 1}
			at #"-" => return #{1, -1}
			else => return #{0, 1}
		}
	}

	on [
		getBaseFromStr: str (Str)
		usesUnifiedPrefix: (Bool)
		index: (Int)
	] (Tuple[Int, Int]) is static is hidden {
		my base = {
			if usesUnifiedPrefix {
				match str[at: index++] at #"0" <= my d1 <= #"9" {
					match str[at: index++] {
						at #"0" <= my d2 <= #"9" {
							if str[at: index++] != #"r" {
								throw "Parse error"
							}

							if (d1 ?= #"0" && d2 < #"2") || (d1 ?= #"3" && d2 > #"6") {
								throw "Invaid base: must be 2..36"
							}
						}
						
						at #"r" {
							if d1 < #"2" {
								throw "Invaid base: must be 2..36"
							} else {
								return d1[Int] - 48
							}
						}

						else => throw "Parse error"
					}
				} else {
					throw "Parse error"
				}
			} else {
				if str[at: index++] != #"0" {
					throw "Expected prefix"
				}

				match str[at: index++] {
					at #"b" => return 2
					at #"o" => return 8
					at #"d" => return 10
					at #"x" => return 16
					else => throw "Invalid base prefix"
				}
			}
		}

		return #{base, index}
	}

	on [
		fromStr: str (Str)
		withBase: base (Int)
		sign: (Int)
		index: (Int)
	] (Int) is static is hidden {
		if base < 10 {
			my max = [base - 1 + 48 Char]
			my int = 0

			while index < str.length {
				match str[at: index++] at #"0" <= my char <= max {
					int *= base
					int += char[Int] - 48
				} else {
					throw "Parse error"
				}
			}

			return int
		} else {
			my maxUpper = [base - 1 + 55 Char]
			my maxLower = [base - 1 + 87 Char]
			my int = 0

			while index < str.length {
				int *= base
				int += {
					match str[at: index++] {
						at #"0" <= my char <= #"9" => return char[Int] - 48
						at #"A" <= my char <= maxUpper => return char[Int] - 55
						at #"a" <= my char <= maxLower => return char[Int] - 87
						else => throw "Parse error"
					}
				}
			}

			return int
		}
	}


	on [
		toBase: base (Int)
		includePrefix: (Bool) = false
		useUnifiedPrefix: (Bool) = false
		minDigits: (Int) = 1
		isUpper: (Bool) = false
	] (Str) {
		if base < 2 || base > 36 {
			throw "Invalid base: must be 2..36"
		}

		my result = [this < 0 yes: "-" no: ""]

		if includePrefix {
			if useUnifiedPrefix {
				result
				-> [add: base]
				-> [add: #"r"]
			} else {
				result
				-> [add: #"0"]
				-> [add: {
					match base {
						at 2 => return #"b"
						at 8 => return #"o"
						at 10 => return #"d"
						at 16 => return #"x"
						else => throw "Base \(base) does not have a prefix!"
					}
				}]
			}
		}

		my int = this[abs]
		my digits = {
			case {
				at int ?= 0 => return "0"
				at base ?= 10 => return int[Str]
				else => return "" -> {
					my alphaStart = isUpper[yes: #"A" no: #"a"]

					while int > 0 {
						my digit = int % base

						this[add: [digit < 10 yes: #"0" no: alphaStart] + digit]
						
						int //= base
					}
				}
			}
		}

		while digits.length < minDigits {
			digits[prepend: #"0"]
		}

		result[add: digits]

		return result
	}

	on [digitLength] (Int) is inline {
		return this[Power digitLength: 10]
	}

	on [digitLength: base (Int)] (Int) {
		if this ?= 0 {
			return 1
		} else {
			return [this[abs] + 1 log: base][ceiling][Int]
		}
	}

	on [digits] (Array[Int]) is inline {
		return this[Power digits: 10]
	}

	on [digits: base (Int)] (Array[Int]) {
		if this ?= 0 {
			return #[0]
		} else {
			my int = this[abs]
			my digits = #[]

			while int != 0 {
				digits[add: int % base]
				int //= base
			}

			if this < 0 {
				digits[at: -1] *= -1
			}

			return digits
		}
	}

	on [digitAt: position (Int)] (Int) is inline {
		return this[Power digitAt: position withBase: 10]
	}

	on [digitAt: position (Int) withBase: base (Int)] (Int) {
		return (this // base ** position) % base
	}

	on [nth] (Str) {
		return this[Str] + {
			if 11 <= this % 100 <= 13 {
				return "th"
			} else {
				match this % 10 {
					at 1 => return "st"
					at 2 => return "nd"
					at 3 => return "rd"
					else => return "th"
				}
			}
		}
	}


	;-- Other things to look at adding:
	; https://github.com/russellallen/self/blob/master/objects/core/integer.self#L401
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L134
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L81
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L4
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L380
}
