use Native

class Char of Ordered is native[repr: `int` bits: 8 signed: true] is strong {
	;-- what to do about underflow/overflow...?
	on [next] (This) {
		return this + 1
	}

	on [previous] (This) {
		return this - 1
	}

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


	operator `+` [value (Int)] (This) {
		return [this[UInt8] + value[UInt8] This]
	}

	operator `-` [value (Int)] (This) {
		return [this[UInt8] - value[UInt8] This]
	}

	operator `?=` [char (This)] (Bool) is native `u8_eq`
	operator `!=` [char (This)] (Bool) is native `u8_ne`
	operator `>` [char (This)] (Bool) is native `u8_gt`
	operator `>=` [char (This)] (Bool) is native `u8_ge`
	operator `<` [char (This)] (Bool) is native `u8_lt`
	operator `<=` [char (This)] (Bool) is native `u8_le`

	
	on [Int8] is native `cast_u8_i8`
	on [Int16] is native `cast_u8_i16`
	on [UInt16] is native `cast_u8_u16`
	on [Int32] is native `cast_u8_i32`
	on [UInt32] is native `cast_u8_u32`
	on [Int64] is native `cast_u8_i64`
	on [UInt64] is native `cast_u8_u64`
	on [Int128] is native `cast_u8_i128`
	on [UInt128] is native `cast_u8_u128`
	on [Int] is native `cast_u8_i32`
	on [Str] is native `cast_char_str`
}