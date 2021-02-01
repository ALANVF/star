use Core

category Power for Int {
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
						else => throw "Base \(b) does not have a prefix!"
					}
				}]
			}
		}

		my int = this[abs]
		my digits = {
			if int ?= 0 {
				return "0"
			} orif base ?= 10 {
				return int[Str]
			} else {
				return "" -> {
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
		return this[digitLength: 10]
	}

	on [digitLength: base (Int)] (Int) {
		if this ?= 0 {
			return 1
		} else {
			return [this[abs] + 1 log: base][ceiling][Int]
		}
	}

	on [digits] (Array[Int]) is inline {
		return this[digits: 10]
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
		return this[digitAt: position withBase: 10]
	}

	on [digitAt: position (Int) withBase: base (Int)] (Int) {
		return this // base ** exponent % base
	}


	;-- Other things to look at adding:
	; https://github.com/russellallen/self/blob/master/objects/core/integer.self#L401
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L134
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L81
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L4
	; https://github.com/pharo-project/pharo/blob/Pharo9.0/src/Math-Operations-Extensions/Integer.extension.st#L380
}