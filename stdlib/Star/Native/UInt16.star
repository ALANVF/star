class UInt16 of Core.Ordered is native[repr: `int` bits: 16 signed: false] {
	on [next] (This) is native `u16_succ`
	on [previous] (This) is native `u16_pred`

	
	operator `-` (This) is native `u16_neg`
	operator `~` (This) is native `u16_compl`
	
	operator `+` [int (This)] (This) is native `u16_add`
	operator `-` [int (This)] (This) is native `u16_sub`
	operator `*` [int (This)] (This) is native `u16_mult`
	operator `**` [int (This)] (This) is native `u16_pow`
	operator `/` [int (This)] (Core.Dec) is native `u16_div`
	operator `//` [int (This)] (This) is native `u16_idiv`
	operator `%` [int (This)] (This) is native `u16_mod`
	operator `%%` [int (This)] (Core.Bool) is native `u16_mod0`
	operator `&` [int (This)] (This) is native `u16_and`
	operator `|` [int (This)] (This) is native `u16_or`
	operator `^` [int (This)] (This) is native `u16_xor`
	operator `<<` [int (This)] (This) is native `u16_shl`
	operator `>>` [int (This)] (This) is native `u16_shr`
	operator `?=` [int (This)] (Core.Bool) is native `u16_eq`
	operator `!=` [int (This)] (Core.Bool) is native `u16_ne`
	operator `>` [int (This)] (Core.Bool) is native `u16_gt`
	operator `>=` [int (This)] (Core.Bool) is native `u16_ge`
	operator `<` [int (This)] (Core.Bool) is native `u16_lt`
	operator `<=` [int (This)] (Core.Bool) is native `u16_le`


	on [Core.Bool] is native `cast_u16_bool`
	on [Int8] is native `cast_u16_i8`
	on [UInt8] is native `cast_u16_u8`
	on [Int16] is native `cast_u16_i16`
	on [Int32] is native `cast_u16_i32`
	on [UInt32] is native `cast_u16_u32`
	on [Int64] is native `cast_u16_i64`
	on [UInt64] is native `cast_u16_u64`
	on [Core.Dec] is native `cast_u16_d64`
	on [Core.Str] is native `cast_u16_str`
}