class UInt8 of Core.Ordered is native[repr: `int` bits: 8 signed: false] {
	on [next] (This) is native `u8_succ`
	on [previous] (This) is native `u8_pred`

	
	operator `?` (Core.Bool) is native `u8_truthy`
	operator `-` (This) is native `u8_neg`
	operator `~` (This) is native `u8_compl`
	
	operator `+` [int (This)] (This) is native `u8_add`
	operator `-` [int (This)] (This) is native `u8_sub`
	operator `*` [int (This)] (This) is native `u8_mult`
	operator `**` [int (This)] (This) is native `u8_pow`
	operator `/` [int (This)] (Core.Dec) is native `u8_div`
	operator `//` [int (This)] (This) is native `u8_idiv`
	operator `%` [int (This)] (This) is native `u8_mod`
	operator `%%` [int (This)] (Core.Bool) is native `u8_mod0`
	operator `&` [int (This)] (This) is native `u8_and`
	operator `|` [int (This)] (This) is native `u8_or`
	operator `^` [int (This)] (This) is native `u8_xor`
	operator `<<` [int (This)] (This) is native `u8_shl`
	operator `>>` [int (This)] (This) is native `u8_shr`
	operator `?=` [int (This)] (Core.Bool) is native `u8_eq`
	operator `!=` [int (This)] (Core.Bool) is native `u8_ne`
	operator `>` [int (This)] (Core.Bool) is native `u8_gt`
	operator `>=` [int (This)] (Core.Bool) is native `u8_ge`
	operator `<` [int (This)] (Core.Bool) is native `u8_lt`
	operator `<=` [int (This)] (Core.Bool) is native `u8_le`


	on [Core.Bool] is native `cast_u8_bool`
	on [Int8] is native `cast_u8_i8`
	on [Int16] is native `cast_u8_i16`
	on [UInt16] is native `cast_u8_u16`
	on [Int32] is native `cast_u8_i32`
	on [UInt32] is native `cast_u8_u32`
	on [Int64] is native `cast_u8_i64`
	on [UInt64] is native `cast_u8_u64`
	on [Core.Dec] is native `cast_u8_d64`
	on [Core.Str] is native `cast_u8_str`
}