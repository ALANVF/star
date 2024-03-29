use Native

class Char of Ordered is native[repr: `int` bits: 8 signed: false] is strong {
	;== Casing

	on [lowercase] (This) {
		if #"A" <= this <= #"Z" {
			return this + 32
		} else {
			return this
		}
	}

	on [uppercase] (This) {
		if #"a" <= this <= #"z" {
			return this - 32
		} else {
			return this
		}
	}


	;== Traits

	on [isDigit] (Bool) is inline {
		return #"0" <= this <= #"9"
	}

	on [isSpace] (Bool) is inline {
		return (
			|| this ?= #" "
			|| #"\t" <= this <= #"\r"
		)
	}

	on [isUpper] (Bool) is inline {
		return #"A" <= this <= #"Z"
	}

	on [isLower] (Bool) is inline {
		return #"a" <= this <= #"z"
	}

	on [isAlpha] (Bool) is inline {
		return this[isUpper] || this[isLower]
	}

	on [isAlnum] (Bool) is inline {
		return this[isAlpha] || this[isDigit]
	}


	;== Escaping

	on [escape] (Str) {
		match this {
			at #"\"" || #"\\" => return "\\\(this)"
			at #"\n" => return "\\n"
			at #"\r" => return "\\r"
			at #"\t" => return "\\t"
			; ...
			at (
				|| 0 <= _ <= 8
				|| 11 <= _ <= 12
				|| 14 <= _ <= 31
			) => return "\\x" + this[Int][Power toBase: 16 minDigits: 2]
			else => return this[Str]
		}
	}


	;== Ranges

	on [to: (This)] (Range[This]) is inline {
		return Range[This][from: this :to]
	}

	;[on [to: (This) by: (Int)] (Range[This]) is inline {
		return Range[This][from: this :to :by]
	}

	on [upto: (This)] (Range[This]) is inline {
		return Range[This][from: this :upto]
	}

	on [upto: (This) by: (Int)] (Range[This]) is inline {
		return Range[This][from: this :upto :by]
	}

	on [downto: (This)] (Range[This]) is inline {
		return Range[This][from: this :downto]
	}
	
	on [downto: (This) by: (Int)] (Range[This]) is inline {
		return Range[This][from: this :downto :by]
	}]


	;== Stepping

	;-- what to do about underflow/overflow...?
	operator `++` (This) => return this + 1
	operator `--` (This) => return this - 1


	;== Math

	operator `+` [value (Int)] (This) => return [this[UInt8] + value[UInt8] This]
	operator `-` [value (Int)] (This) => return [this[UInt8] - value[UInt8] This]

	operator `+` [char (This)] (This) is native `u8_add`
	operator `-` [char (This)] (This) is native `u8_sub`

	
	;== Comparing

	operator `?=` [char (This)] (Bool) is native `u8_eq`
	operator `!=` [char (This)] (Bool) is native `u8_ne`
	operator `>` [char (This)] (Bool) is native `u8_gt`
	operator `>=` [char (This)] (Bool) is native `u8_ge`
	operator `<` [char (This)] (Bool) is native `u8_lt`
	operator `<=` [char (This)] (Bool) is native `u8_le`

	operator `?=` [int (Int)] (Bool) is inline => return this[Int] ?= int
	operator `!=` [int (Int)] (Bool) is inline => return this[Int] != int
	operator `>` [int (Int)] (Bool) is inline => return this[Int] > int
	operator `>=` [int (Int)] (Bool) is inline => return this[Int] >= int
	operator `<` [int (Int)] (Bool) is inline => return this[Int] < int
	operator `<=` [int (Int)] (Bool) is inline => return this[Int] <= int

	
	;== Converting

	on [Int8] is native `cast_u8_i8`
	on [Int16] is native `cast_u8_i16`
	on [UInt16] is native `cast_u8_u16`
	on [Int32] is native `cast_u8_i32`
	on [UInt32] is native `cast_u8_u32`
	on [Int64] is native `cast_u8_i64`
	on [UInt64] is native `cast_u8_u64`
	;on [Int] is native `cast_u8_i32`
	on [Str] is inline => return Str[new: this]
}