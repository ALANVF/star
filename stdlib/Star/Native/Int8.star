class Int8 of Core.Ordered is native[repr: `int` bits: 8 signed: true] {
	on [next] (This) is native `i8_succ`
	on [previous] (This) is native `i8_pred`


	operator `-` (This) is native `i8_neg`
	operator `~` (This) is native `i8_compl`
	
	operator `+` [int (This)] (This) is native `i8_add`
	operator `-` [int (This)] (This) is native `i8_sub`
	operator `*` [int (This)] (This) is native `i8_mult`
	operator `**` [int (This)] (This) is native `i8_pow`
	operator `/` [int (This)] (Core.Dec) is native `i8_div`
	operator `//` [int (This)] (This) is native `i8_idiv`
	operator `%` [int (This)] (This) is native `i8_mod`
	operator `%%` [int (This)] (Core.Bool) is native `i8_mod0`
	operator `&` [int (This)] (This) is native `i8_and`
	operator `|` [int (This)] (This) is native `i8_or`
	operator `^` [int (This)] (This) is native `i8_xor`
	operator `<<` [int (This)] (This) is native `i8_shl`
	operator `>>` [int (This)] (This) is native `i8_shr`
	operator `?=` [int (This)] (Core.Bool) is native `i8_eq`
	operator `!=` [int (This)] (Core.Bool) is native `i8_ne`
	operator `>` [int (This)] (Core.Bool) is native `i8_gt`
	operator `>=` [int (This)] (Core.Bool) is native `i8_ge`
	operator `<` [int (This)] (Core.Bool) is native `i8_lt`
	operator `<=` [int (This)] (Core.Bool) is native `i8_le`


	on [Core.Bool] is native `cast_i8_bool`
	on [UInt8] is native `cast_i8_u8`
	on [Int16] is native `cast_i8_i16`
	on [UInt16] is native `cast_i8_u16`
	on [Int32] is native `cast_i8_i32`
	on [UInt32] is native `cast_i8_u32`
	on [Int64] is native `cast_i8_i64`
	on [UInt64] is native `cast_i8_u64`
	on [Core.Dec] is native `cast_i8_d64`
	on [Core.Str] is native `cast_i8_str`
}