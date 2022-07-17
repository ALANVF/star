use Native

class Dec of Num is native[repr: `dec` bits: 64] is strong {
	;== Constants

	on [nan] (Dec) is static is getter is native `d64_nan`
	on [inf] (Dec) is static is getter is native `d64_inf`
	on [neg_inf] (Dec) is static is getter is native `d64_neg_inf`
	
	on [pi] (Dec) is static is getter is native `d64_pi`


	;== Math

	on [sign] (Dec) {
		case {
			at this < 0.0 => return -1.0
			at this > 0.0 => return 1.0
			else          => return 0.0
		}
	}

	on [abs] (This) is native `d64_abs`
	on [sqrt] (This) is native `d64_sqrt`

	on [rootOf: root (Num)] (This) {
		my x = this / root
		my m = [root - 1 Dec]

		while true {
			my x' = (m * x + this / x ** m) / root

			if [x' - x abs] < [x * 1e-9 abs] => return x'
			
			x = x'
		}
	}
	
	on [exp] (This) is native `d64_exp`
	on [sin] (This) is native `d64_sin`
	on [cos] (This) is native `d64_cos`
	on [tan] (This) is native `d64_tan`
	on [asin] (This) is native `d64_asin`
	on [acos] (This) is native `d64_acos`
	on [atan] (This) is native `d64_atan`
	on [floor] (This) is native `d64_floor`
	on [ceiling] (This) is native `d64_ceil`
	on [truncate] (This) is native `d64_trunc`
	on [round] (This) is native `d64_round`
	
	on [round: digits (Int)] (This) {
		my tens = 10 ** digits

		return [this * tens round] / tens
	}
	
	on [ln] (This) is native `d64_ln`
	on [log] (This) is native `d64_log`
	
	on [log: base (Int)] (This) {
		return this[log] / base[log]
	}
	
	
	;== Stepping
	
	operator `++` (This) is native `d64_succ`
	operator `--` (This) is native `d64_pred`
	
	
	;== Ranges
	
	on [to: (This)] (Range[This]) is inline {
		return Range[This][from: this :to]
	}

	;[
	on [to: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :to :by]
	}

	on [upto: (This)] (Range[This]) is inline {
		return Range[This][from: this :upto]
	}

	on [upto: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :upto :by]
	}

	on [downto: (This)] (Range[This]) is inline {
		return Range[This][from: this :downto]
	}

	on [downto: (This) by: (This)] (Range[This]) is inline {
		return Range[This][from: this :downto :by]
	}
	]


	;== Operators

	operator `?` (Bool) is native `d64_truthy`
	operator `-` (This) is native `d64_neg`
	
	operator `+` [other (This)] (This) is native `d64_add`
	operator `+` [other (Num)] (This) => return this + other[Dec]
	
	operator `-` [other (This)] (This) is native `d64_sub`
	operator `-` [other (Num)] (This) => return this - other[Dec]
	
	operator `*` [other (This)] (This) is native `d64_mult`
	operator `*` [other (Num)] (This) => return this * other[Dec]
	
	operator `**` [other (This)] (This) is native `d64_pow`
	operator `**` [other (Num)] (This) => return this ** other[Dec]
	
	operator `/` [other (This)] (This) is native `d64_div`
	operator `/` [other (Num)] (This) => return this / other[Dec]
	
	operator `//` [other (This)] (Int) is native `d64_idiv`
	operator `//` [other (Num)] (Int) => return this // other[Dec]
	
	operator `%` [other (This)] (This) is native `d64_mod`
	operator `%` [other (Num)] (This) => return this % other[Dec]
	
	operator `%%` [other (This)] (Bool) is native `d64_mod0`
	operator `%%` [other (Num)] (Bool) => return this %% other[Dec]
	
	operator `?=` [other (This)] (Bool) is native `d64_eq`
	operator `?=` [other (Num)] (Bool) => return this ?= other[Dec]
	
	operator `!=` [other (This)] (Bool) is native `d64_ne`
	operator `!=` [other (Num)] (Bool) => return this != other[Dec]
	
	operator `>` [other (This)] (Bool) is native `d64_gt`
	operator `>` [other (Num)] (Bool) => return this > other[Dec]
	
	operator `>=` [other (This)] (Bool) is native `d64_ge`
	operator `>=` [other (Num)] (Bool) => return this >= other[Dec]
	
	operator `<` [other (This)] (Bool) is native `d64_lt`
	operator `<` [other (Num)] (Bool) => return this < other[Dec]
	
	operator `<=` [other (This)] (Bool) is native `d64_le`
	operator `<=` [other (Num)] (Bool) => return this <= other[Dec]



	;== Converting
	
	on [Int8] is native `cast_d64_i8`
	on [UInt8] is native `cast_d64_u8`
	on [Int16] is native `cast_d64_i16`
	on [UInt16] is native `cast_d64_u16`
	on [Int32] is native `cast_d64_i32`
	on [UInt32] is native `cast_d64_u32`
	on [Int64] is native `cast_d64_i64`
	on [UInt64] is native `cast_d64_u64`
	on [Int] is native `cast_d64_i32`
	on [Dec] is inline => return this
	on [Str] is native `cast_d64_str`
}