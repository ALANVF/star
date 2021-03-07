use Native

class Bool of Comparable is native[repr: `bool`] is strong {
	; These macros exist purely for documentation purposes until Star is
	; bootstrapped and macros are implemented

	type T
	on [yes: (T) no: (T)] (T) is macro {
		if #expand this {
			return #expand yes
		} else {
			return #expand no
		}
	}


	operator `!` (Bool) is native `bool_not`
	
	operator `?` (Bool) is inline {
		return this
	}

	operator `&` [bool (Bool)] (Bool) is native `bool_and`
	operator `|` [bool (Bool)] (Bool) is native `bool_or`
	operator `^` [bool (Bool)] (Bool) is native `bool_xor`
	operator `?=` [bool (Bool)] (Bool) is native `bool_eq`
	operator `!=` [bool (Bool)] (Bool) is native `bool_ne`
	operator `>` [bool (Bool)] (Bool) is native `bool_gt`
	operator `>=` [bool (Bool)] (Bool) is native `bool_ge`
	operator `<` [bool (Bool)] (Bool) is native `bool_lt`
	operator `<=` [bool (Bool)] (Bool) is native `bool_le`

	operator `&&` [cond (Bool)] (Bool) is macro {
		if #expand this {
			return #expand cond
		} else {
			return false
		}
	}

	operator `||` [cond (Bool)] (Bool) is macro {
		if #expand this {
			return true
		} else {
			return #expand cond
		}
	}

	operator `^^` [cond (Bool)] (Bool) is macro {
		match cond at #quote (my left ^^ my right) {
			;-- We don't want to expand `left` twice, so save it for later
			my leftCond = #expand left

			if #expand this != leftCond {
				return leftCond ^^ right
			} else {
				return false
			}
		} else {
			return #expand this != #expand cond 
		}
	}

	operator `!!` [cond (Bool)] (Bool) is macro {
		match cond at #quote (my left !! my right) {
			my result = #expand this || #expand left

			while true {
				match right at #quote (my left' !! my right') {
					result ||= #expand left'
					right = right'
				} else {
					result ||= #expand right'
					break
				}
			}

			return !result
		} else {
			return !(#expand this || #expand cond)
		}
	}


	on [Int8] is native `cast_bool_i8`
	on [UInt8] is native `cast_bool_u8`
	on [Int16] is native `cast_bool_i16`
	on [UInt16] is native `cast_bool_u16`
	on [Int32] is native `cast_bool_i32`
	on [UInt32] is native `cast_bool_u32`
	on [Int64] is native `cast_bool_i64`
	on [UInt64] is native `cast_bool_u64`
	on [Str] is native `cast_bool_str`
	on [Int] is native `cast_bool_i32`
}