use Native

class Bool of Comparable is native[repr: `bool`] is strong {
	;[type T
	macro [yes: (T) no: (T)] (T) is unordered {
		return @{
			{
				if @this {
					return @yes
				} else {
					return @no
				}
			}
		}
	}]


	operator `!` (Bool) is native `bool_not`
	operator `?` (Bool) is native `bool_truthy`

	operator `&` [bool (Bool)] (Bool) is native `bool_and`
	operator `|` [bool (Bool)] (Bool) is native `bool_or`
	operator `^` [bool (Bool)] (Bool) is native `bool_xor`
	operator `?=` [bool (Bool)] (Bool) is native `bool_eq`
	operator `!=` [bool (Bool)] (Bool) is native `bool_ne`
	operator `>` [bool (Bool)] (Bool) is native `bool_gt`
	operator `>=` [bool (Bool)] (Bool) is native `bool_ge`
	operator `<` [bool (Bool)] (Bool) is native `bool_lt`
	operator `<=` [bool (Bool)] (Bool) is native `bool_le`


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